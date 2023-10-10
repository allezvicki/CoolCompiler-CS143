#include "semant.h"
#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "utilities.h"
#include <regex>
#include <stack>
#include <sys/types.h>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string,
    IO, length, Main, main_meth, No_class, No_type, Object, out_int, out_string,
    prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

/* Utilities for type checking */

// Identifier -> Type curr_table (For the Second Pass)
// How to deal with multiple lets: a stack.
std::unordered_map<Symbol, std::stack<Symbol>> ids;

// The Class we are in (For SELF_TYPE)
Symbol curr_class;

// Global classtable for error messages
ClassTableP curr_table;
Symbol curr_file;

// If id exists, returns type, else returns No_type
Symbol id_exists(Symbol id) {
    auto iter = ids.find(id);
    if (iter != ids.end() && iter->second.size()) {
        // id exists
        return iter->second.top();
    }
    return No_type;
}

// Mapping of Symbol of class to ClassNode
// types include: basic classes and user defined classes (No No_type etc)
std::unordered_map<Symbol, ClassNode *> symbol2node;

// Check if two Classes are ancestor and child
// Ancestor and child must be valid types
bool ancestor_and_child(Symbol ancestor, Symbol child) {
    if (child == No_type) {
        return true;
    }
    // Child is not No_type
    if (ancestor == No_type) {
        return false;
    }
    if (ancestor == SELF_TYPE) {
        if (child != SELF_TYPE) {
            return false;
        }
        ancestor = curr_class;
    }
    if (child == SELF_TYPE) {
        child = curr_class;
    }
    // Both are valid types
    if (child == ancestor) {
        return true;
    }
    auto child_node = symbol2node[child];
    return child_node->has_ancestor(symbol2node[ancestor]);
}

bool type_exists(Symbol type) {
    return symbol2node.find(type) != symbol2node.end();
}

Symbol get_lca(Symbol u, Symbol v) {
    if (u == No_type) {
        return v;
    }
    if (v == No_type) {
        return u;
    }
    if (u == SELF_TYPE) {
        u = curr_class;
    }
    if (v == SELF_TYPE) {
        v = curr_class;
    }
    return symbol2node[u]->get_lca(symbol2node[v]);
}

Method *get_method(Symbol class_name, Symbol method_name) {
    if (class_name == SELF_TYPE) {
        class_name = curr_class;
    }
    auto class_node = symbol2node[class_name];
    return class_node->get_method(method_name);
}

/* ClassTable */

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr) {

    /* Fill this in */
    /* Build my class tree here! */

    /* _class_root (Object_class) now with all the basic classes */
    _class_root = install_basic_classes();

    // Fist scan: add all the edges
    //
    // Second scan: build the tree from the edges, when building,
    // test if all the edges form a valid tree (without cycles and
    // unused edges)

    // parent -> children mapping
    std::unordered_map<Symbol, std::vector<class__class *>> edges;

    // Every class name can only appear once as a child
    // vis should also prevent redefinition of basic classes
    std::unordered_set<Symbol> vis;
    vis.insert(Object);
    vis.insert(Str);
    vis.insert(Int);
    vis.insert(Bool);
    vis.insert(SELF_TYPE);
    vis.insert(No_type);
    vis.insert(prim_slot);

    // Ensure all edges are valid (in tree) -- some edges may not be valid
    // because parent is undefined
    int tot_edges = 0;
    int expected_edges = 0;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto clss = dynamic_cast<class__class *>(classes->nth(i));
        auto parent = clss->get_parent();
        edges[parent].push_back(clss);
        expected_edges++;
        // Chack if basic classes are inherited
        if (parent == Int || parent == Str || parent == Bool ||
            parent == SELF_TYPE) {
            semant_error(clss)
                << "cannot inherit from Int / Bool / Str / SELF_TYPE\n";
            return;
        }
    }

    // A set of methods that are inherited by the class
    std::unordered_map<Symbol, std::vector<Symbol>> mtds;

    // A set of attributes that are inherited
    std::unordered_set<Symbol> attrs;

    try {
        _class_root->build(edges, vis, tot_edges, mtds, attrs);
    } catch (CoolError &ce) {
        // Caught "class redefined" error and other check_feature error
        semant_error(curr_file, ce.node) << ce.msg << endl;
        return;
    }

    // Encountered cycle buiding inheritance graph
    // TODO: Imporve error reporting here...
    // TODO: delete edges
    /* if (tot_edges != expected_edges) { */
    /*     semant_error() << "parent class undefined\n"; */
    /* } */

    for (int i = 0; i < classes->len(); i++) {
        auto parent =
            dynamic_cast<class__class *>(classes->nth(i))->get_parent();
        if (!type_exists(parent)) {
            semant_error(classes->nth(i))
                << "class inherits from an undefined class " << parent << endl;
        }
    }

    // Main class should be defined
    // TODO: This should be done at last step
    /* if (vis.find(Main) == vis.end()) { */
    /*     semant_error() << "class Main undefined\n"; */
    /* } */
}

ClassNode *ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class = class_(
        Object, No_class,
        append_Features(
            append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                   Object, no_expr())),
                            single_Features(method(type_name, nil_Formals(),
                                                   Str, no_expr()))),
            single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
        filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = class_(
        IO, Object,
        append_Features(
            append_Features(
                append_Features(
                    single_Features(method(out_string,
                                           single_Formals(formal(arg, Str)),
                                           SELF_TYPE, no_expr())),
                    single_Features(method(out_int,
                                           single_Formals(formal(arg, Int)),
                                           SELF_TYPE, no_expr()))),
                single_Features(
                    method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
        filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
        Str, Object,
        append_Features(
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(attr(val, Int, no_expr())),
                        single_Features(attr(str_field, prim_slot, no_expr()))),
                    single_Features(
                        method(length, nil_Formals(), Int, no_expr()))),
                single_Features(method(concat, single_Formals(formal(arg, Str)),
                                       Str, no_expr()))),
            single_Features(
                method(substr,
                       append_Formals(single_Formals(formal(arg, Int)),
                                      single_Formals(formal(arg2, Int))),
                       Str, no_expr()))),
        filename);

    ClassNode *class_root = new ClassNode{Object_class};
    /* symbol2node[Object] = class_root; */

    /* auto node = new ClassNode{Str_class, class_root}; */
    /* symbol2node[Str] = node; */
    class_root->add_child(new ClassNode{Str_class, class_root});

    /* node = new ClassNode{Bool_class, class_root}; */
    /* symbol2node[Bool] = node; */
    /* class_root->add_child(node); */
    class_root->add_child(new ClassNode{Bool_class, class_root});

    /* node = new ClassNode{Int_class, class_root}; */
    /* symbol2node[Int] = node; */
    /* class_root->add_child(node); */
    class_root->add_child(new ClassNode{Int_class, class_root});

    /* node = new ClassNode{IO_class, class_root}; */
    /* symbol2node[IO] = node; */
    /* class_root->add_child(node); */
    class_root->add_child(new ClassNode{IO_class, class_root});

    return class_root;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

void ClassTable::recurse() {
    curr_table = this;
    _class_root->recurse();
}

void ClassTable::check_types() {
    curr_table = this;
    _class_root->check_types();
}

/* ClassNode */

bool ClassNode::has_ancestor(ClassNode *anc) {
    auto now = this;
    while (now != NULL) {
        if (now == anc) {
            return true;
        }
        now = now->_parent;
    }
    return false;
}

Symbol ClassNode::get_lca(ClassNode *v) {
    auto u = this;
    if (u->_height < v->_height)
        std::swap(u, v);
    // u is deeper
    while (u->_height > v->_height) {
        u = u->_parent;
    }
    while (u != v) {
        u = u->_parent;
        v = v->_parent;
    }
    return dynamic_cast<class__class *>(u->_class)->get_name();
}

Method *ClassNode::get_method(Symbol method_name) {
    auto now = this;
    while (now != NULL) {
        for (auto &method : now->_methods) {
            if (method.name == method_name) {
                return &method;
            }
        }
        now = now->_parent;
    }
    return NULL;
}

void ClassNode::build(
    std::unordered_map<Symbol, std::vector<class__class *>> &edges,
    std::unordered_set<Symbol> &vis, int &tot,
    std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
    std::unordered_set<Symbol> &attrs) {

    Symbol class_name = dynamic_cast<class__class *>(_class)->get_name();
    curr_file = _class->get_filename();
    // Add name to node mapping
    symbol2node[class_name] = this;

    // Add methods and check attributes
    check_features(mtds, attrs, _methods, _attrs);

    // If Object, also recurse basic types
    if (class_name == Object) {
        for (auto child = _children; child; child = child->tl()) {
            child->hd()->build(edges, vis, tot, mtds, attrs);
        }
    }

    // Add children
    for (auto child_class : edges[class_name]) {
        // symbol appears twice, class redefind
        // Or basic class (Int, Bool, Str, No_type...) got defined
        if (vis.find(child_class->get_name()) != vis.end()) {
            throw CoolError{child_class, "class redefined"};
        }
        vis.insert(child_class->get_name());

        auto child_node = new ClassNode{child_class, this};
        child_node->_height = _height + 1;
        add_child(child_node);

        if (semant_debug) {
            cerr << "build edge: "
                 << dynamic_cast<class__class *>(_class)->get_name() << " "
                 << child_class->get_name() << endl;
        }

        tot++;
        child_node->build(edges, vis, tot, mtds, attrs);
    }

    // Remove newly defined methods and attributes
    for (auto &mtd : _methods) {
        mtds.erase(mtd.name);
    }
    for (auto &attr : _attrs) {
        attrs.erase(attr);
    }
}

void ClassNode::add_child(ClassNode *child) {
    _children = new List<ClassNode>(child, _children);
}

void ClassNode::check_features(
    std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
    std::unordered_set<Symbol> &attrs, std::vector<Method> &new_mtds,
    std::vector<Symbol> &new_attrs) {
    dynamic_cast<class__class *>(_class)->check_features(mtds, attrs, new_mtds,
                                                         new_attrs);
}

void ClassNode::recurse() {
    auto clss = dynamic_cast<class__class *>(_class);
    curr_class = clss->get_name();
    curr_file = clss->get_filename();

    // Check if Main has method main
    if (curr_class == Main) {
        bool has_main = false;
        for (auto &method : _methods) {
            if (method.name == main_meth) {
                has_main = true;
            }
        }
        if (!has_main) {
            curr_table->semant_error(curr_file, _class)
                << "class Main does no have method main\n";
        }
    }

    _class->recurse();
    for (auto child = _children; child; child = child->tl()) {
        child->hd()->recurse();
    }
    // Remove attributes
    for (auto &attr : _attrs) {
        ids[attr].pop();
    }
}

void ClassNode::check_types() {
    for (auto &method : _methods) {
        for (auto &type : method.types) {
            if (!type_exists(type) && type != SELF_TYPE) {
                type = No_type;
                // This will be reported when recursing the method
                /* curr_table->semant_error(_class) << method.name << ": method
                 * argument/return type undefined\n"; */
            }
        }
    }
}

/* new methods added to cool-tree.h */

Symbol formal_class::get_type() { return type_decl; }
Symbol formal_class::get_name() { return name; }

/* check_features for attribute and method */
void class__class::check_features(
    std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
    std::unordered_set<Symbol> &attrs, std::vector<Method> &new_mtds,
    std::vector<Symbol> &new_attrs) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature f = features->nth(i);
        f->check_feature(mtds, attrs, new_mtds, new_attrs);
    }
}

void method_class::check_feature(
    std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
    std::unordered_set<Symbol> &attrs, std::vector<Method> &new_mtds,
    std::vector<Symbol> &new_attrs) {
    auto iter = mtds.find(name);
    if (iter != mtds.end()) {
        // Check if types are correct
        if (iter->second.size() - 1 != formals->len()) {
            throw CoolError{this,
                            "overriding method has different number of types"};
        }
        for (int i = formals->first(), j = 0; formals->more(i);
             i = formals->next(i), j++) {
            auto fo = formals->nth(i);
            auto type = dynamic_cast<formal_class *>(fo)->get_type();
            if (type != iter->second[j]) {
                // Different type...
                throw CoolError{
                    fo, "overriding method has different argument type"};
            }
        }
        if (return_type != iter->second[iter->second.size() - 1]) {
            throw CoolError{this,
                            "overriding method has different return type"};
        }
    } else {
        // Add new method
        std::vector<Symbol> types;
        for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
            auto fo = formals->nth(i);
            auto type = dynamic_cast<formal_class *>(fo)->get_type();
            types.push_back(type);
            if (i < formals->len() - 1 && type == SELF_TYPE) {
                throw CoolError{fo, "method parameter cannot be SELF_TYPE"};
            }
        }
        types.push_back(return_type);
        mtds[name] = types;
        new_mtds.emplace_back(name, std::move(types));
    }
}

void attr_class::check_feature(
    std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
    std::unordered_set<Symbol> &attrs, std::vector<Method> &new_mtds,
    std::vector<Symbol> &new_attrs) {
    if (attrs.find(name) != attrs.end()) {
        // Attribute redefined, throw exception
        throw CoolError{this, "attribute redefined"};
    }
    if (name == self) {
        throw CoolError{this, "'self' cannot be the name of an attribute"};
    }
    attrs.insert(name);
    new_attrs.push_back(name);
}

/* RECURESE */
/* Below describes the second pass, done by xxxx_class::recurse() */
void class__class::recurse() {
    for (int i = 0; i < features->len(); i++) {
        features->nth(i)->recurse();
    }
}

void formal_class::recurse() {
    if (name == self) {
        curr_table->semant_error(curr_file, this)
            << "formal: 'self' cannot be a parameter name\n";
    }
    if (type_exists(type_decl)) {
        ids[name].push(type_decl);
    } else {
        curr_table->semant_error(curr_file, this) << "formal: type undefined\n";
        ids[name].push(No_type);
    }
}

void formal_class::after_recurse() { ids[name].pop(); }

void attr_class::recurse() {
    // If it is a basic class, don't do nothing
    if (curr_class == Int || curr_class == Bool || curr_class == Str) {
        return;
    }
    // init could be no_expr, let it be of type No_type
    if (type_exists(type_decl)) {
        auto init_type = init->recurse();
        if (!ancestor_and_child(type_decl, init_type)) {
            curr_table->semant_error(curr_file, this)
                << "attribute: init type cannot turn into delared type\n";
        }
        ids[name].push(type_decl);
    } else {
        curr_table->semant_error(curr_file, this)
            << "attribute: type undefined\n";
        ids[name].push(No_type);
    }
}

void method_class::recurse() {
    // warn about duplicate formals:
    std::unordered_set<Symbol> names;
    for (int i = 0; i < formals->len(); i++) {
        formals->nth(i)->recurse();
        auto name = formals->nth(i)->get_name();
        if (names.find(name) != names.end()) {
            curr_table->semant_error(curr_file, this)
                << "formal: duplicate parameter name " << name << endl;
        } else {
            names.insert(name);
        }
    }
    Symbol expr_type = expr->recurse();

    if (!type_exists(return_type) && return_type != SELF_TYPE) {
        curr_table->semant_error(curr_file, this)
            << "method: return type " << return_type << " undefined\n";
    } else if (!ancestor_and_child(return_type, expr_type)) {
        curr_table->semant_error(curr_file, this)
            << "method: expression cannot turn into return type\n";
    }
    // Remove formals classes
    for (int i = 0; i < formals->len(); i++) {
        formals->nth(i)->after_recurse();
    }
}

Symbol branch_class::recurse() {
    if (type_exists(type_decl)) {
        ids[name].push(type_decl);
    } else {
        curr_table->semant_error(curr_file, this) << "branch: type undefiend\n";
        ids[name].push(No_type);
    }
    auto type = expr->recurse();
    ids[name].pop();
    return type;
}

// Requirements:
// 1. Identifier declared
// 2. Identifier type is the same as assign expr type
// 3. What if Identifier is self???
Symbol assign_class::recurse() {
    auto id_type = id_exists(name);
    if (id_type == No_type) {
        /* throw CoolError{this, "assign: identifier undefined"}; */
        curr_table->semant_error(curr_file, this)
            << "assign: identifier undefined\n";
    }
    auto expr_type = expr->recurse();
    if (!ancestor_and_child(id_type, expr_type)) {
        /* throw CoolError{this, "assign: expression type not of id type"}; */
        curr_table->semant_error(curr_file, this)
            << "assign: expr type cannot turn into id type\n";
        set_type(No_type);
        return No_type;
    }
    set_type(expr_type);
    return expr_type;
}

// Requirements:
// I'm tired!
// name: method name
// expr: this thing's method we gonna use
// actuals: the arguments
//
Symbol static_dispatch_class::recurse() {
    {
        auto object_type = expr->recurse();
        if (!type_exists(type_name)) {
            // will not find a dispatch, just ignore every thing
            // SELF_TYPE also dimissed
            curr_table->semant_error(curr_file, this)
                << "static dispatch: dispatch type undefined\n";
            goto error;
        }
        // Type exists
        // lca(object_type, dispatch_type) must be dispatch_type
        if (get_lca(object_type, type_name) != type_name) {
            curr_table->semant_error(curr_file, this)
                << "static dispatch: expression type must inherit from "
                   "dispatch type\n";
            goto error;
        }
        // dispatch type valid, find the method
        auto method = get_method(type_name, name);
        if (method == NULL) {
            curr_table->semant_error(curr_file, this)
                << "static dispatch: method undefined\n";
            goto error;
        }
        // Method found!
        if (method->types.size() - 1 != actual->len()) {
            curr_table->semant_error(curr_file, this)
                << "static dispatch: wrong argument number\n";
            goto error;
        }
        bool args_alright{true};
        for (int i = 0; i < actual->len(); i++) {
            auto arg_type = actual->nth(i)->recurse();
            // argument type exsits or not???
            if (!ancestor_and_child(method->types[i], arg_type)) {
                curr_table->semant_error(curr_file, this)
                    << "static dispatch: argument " << i + 1
                    << " of wrong type\n";
                args_alright = false;
            }
        }
        if (args_alright) {
            set_type(method->types[method->types.size() - 1]);
            if (type == SELF_TYPE) {
                type = object_type;
            }
            return type;
        }
    }
error:
    set_type(No_type);
    return No_type;
}

Symbol dispatch_class::recurse() {
    {
        // get expr type
        auto object_type = expr->recurse();
        // expr maybe no_expr, in which case object_type should be SELF_TYPE
        if (object_type == No_type) {
            object_type = SELF_TYPE;
        }
        // get method
        auto method = get_method(object_type, name);
        if (method == NULL) {
            curr_table->semant_error(curr_file, this)
                << "dispatch: method undefined\n";
            goto error;
        }
        // Method found!
        if (method->types.size() - 1 != actual->len()) {
            cout << method->name << endl;
            curr_table->semant_error(curr_file, this)
                << "dispatch: wrong argument number. Should be "
                << method->types.size() - 1 << " is " << actual->len() << endl;
            goto error;
        }
        bool args_alright{true};
        for (int i = 0; i < actual->len(); i++) {
            auto arg_type = actual->nth(i)->recurse();
            if (!ancestor_and_child(method->types[i], arg_type)) {
                curr_table->semant_error(curr_file, this)
                    << "dispatch: argument " << i + 1 << " of wrong type. "
                    << "Should be " << method->types[i] << " is " << arg_type
                    << endl;
                args_alright = false;
            }
        }
        if (args_alright) {
            set_type(method->types[method->types.size() - 1]);
            if (type == SELF_TYPE) {
                type = object_type;
            }
            return type;
        }
    }
error:
    set_type(No_type);
    return No_type;
}

// Requirements:
// 1. pred of type Bool
// 2. return type is LCA of branches
Symbol cond_class::recurse() {
    if (!ancestor_and_child(Bool, pred->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "condition: pred not of type Bool\n";
    }
    auto then_type = then_exp->recurse();
    auto else_type = else_exp->recurse();
    auto lca = get_lca(then_type, else_type);
    set_type(lca);
    return lca;
}

Symbol loop_class::recurse() {
    auto pred_type = pred->recurse();
    if (!ancestor_and_child(Bool, pred_type)) {
        curr_table->semant_error(curr_file, this)
            << "loop: pred not of type Bool\n";
    }
    body->recurse();
    set_type(Object);
    return Object;
}

Symbol branch_class::get_casetype() { return type_decl; }

// Requirements:
// Classes should not duplicate.
Symbol typcase_class::recurse() {
    auto lca = No_type;
    expr->recurse();
    std::unordered_set<Symbol> case_types;
    for (int i = 0; i < cases->len(); i++) {
        auto branch_type = cases->nth(i)->recurse();
        auto case_type = cases->nth(i)->get_casetype();
        if (case_types.find(case_type) != case_types.end()) {
            curr_table->semant_error(curr_file, this)
                << "case branch: duplicate branch " << case_type
                << " in case statement\n";
        } else {
            case_types.insert(case_type);
        }
        lca = get_lca(lca, branch_type);
    }
    set_type(lca);
    return lca;
}

Symbol block_class::recurse() {
    Symbol ret = No_type;
    for (int i = 0; i < body->len(); i++) {
        ret = body->nth(i)->recurse();
    }
    set_type(ret);
    return ret;
}

Symbol let_class::recurse() {
    if (type_exists(type_decl) || type_decl == SELF_TYPE) {
        auto init_type = init->recurse();
        if (!ancestor_and_child(type_decl, init_type)) {
            curr_table->semant_error(curr_file, this)
                << "let: init type cannot turn into declared type\n";
        }
        ids[identifier].push(type_decl);
        if (identifier == self) {
            curr_table->semant_error(curr_file, this)
                << "let: 'self' cannot be let identifier type\n";
        }
    } else {
        curr_table->semant_error(curr_file, this) << "let: type undefiend\n";
        ids[identifier].push(No_type);
    }
    auto body_type = body->recurse();
    set_type(body_type);
    ids[identifier].pop();
    return body_type;
}

Symbol plus_class::recurse() {
    if (!ancestor_and_child(Int, e1->recurse()) ||
        !ancestor_and_child(Int, e2->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "plus: expression(s) no of type int\n";
        set_type(No_type);
    } else {
        set_type(Int);
    }
    return type;
}

Symbol sub_class::recurse() {
    if (!ancestor_and_child(Int, e1->recurse()) ||
        !ancestor_and_child(Int, e2->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "sub: expression(s) no of type int\n";
        set_type(No_type);
    } else {
        set_type(Int);
    }
    return type;
}

Symbol mul_class::recurse() {
    if (!ancestor_and_child(Int, e1->recurse()) ||
        !ancestor_and_child(Int, e2->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "mul: expression(s) no of type int\n";
        set_type(No_type);
    } else {
        set_type(Int);
    }
    return type;
}

Symbol divide_class::recurse() {
    if (!ancestor_and_child(Int, e1->recurse()) ||
        !ancestor_and_child(Int, e2->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "divide: expression(s) no of type int\n";
        set_type(No_type);
    } else {
        set_type(Int);
    }
    return type;
}

Symbol neg_class::recurse() {
    set_type(e1->recurse());
    if (!ancestor_and_child(Int, type)) {
        curr_table->semant_error(curr_file, this)
            << "neg: expression not of type int\n";
        set_type(No_type);
    }
    return type;
}

Symbol lt_class::recurse() {
    auto type1 = e1->recurse();
    auto type2 = e2->recurse();
    if (!ancestor_and_child(Int, type1)) {
        curr_table->semant_error(curr_file, this)
            << "lq: expreesion 1 not of type int";
    }
    if (!ancestor_and_child(Int, type2)) {
        curr_table->semant_error(curr_file, this)
            << "lq: expreesion 2 not of type int";
    }
    set_type(Bool);
    return Bool;
}

Symbol eq_class::recurse() {
    auto type1 = e1->recurse();
    auto type2 = e2->recurse();
    if ((type1 == Int || type1 == Str || type1 == Bool) &&
        !ancestor_and_child(type1, type2)) {
        curr_table->semant_error(curr_file, this)
            << "equal: expreesion 1 of type Int/String/Bool but expression 2 "
               "not of the same type\n";
    } else if ((type2 == Int || type2 == Str || type2 == Bool) &&
               !ancestor_and_child(type2, type1)) {
        curr_table->semant_error(curr_file, this)
            << "equal: expreesion 2 of type Int/String/Bool but expression 1 "
               "not of the same type\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol leq_class::recurse() {
    auto type1 = e1->recurse();
    auto type2 = e2->recurse();
    if (!ancestor_and_child(Int, type1)) {
        curr_table->semant_error(curr_file, this)
            << "leq: expreesion 1 not of type int";
    }
    if (!ancestor_and_child(Int, type2)) {
        curr_table->semant_error(curr_file, this)
            << "leq: expreesion 2 not of type int";
    }
    set_type(Bool);
    return Bool;
}

Symbol comp_class::recurse() {
    if (!ancestor_and_child(Bool, e1->recurse())) {
        curr_table->semant_error(curr_file, this)
            << "not: expression not of type bool\n";
    }
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::recurse() {
    set_type(Int);
    return Int;
}

Symbol bool_const_class::recurse() {
    set_type(Bool);
    return Bool;
}

Symbol string_const_class::recurse() {
    set_type(Str);
    return Str;
}

Symbol new__class::recurse() {
    if (type_name == SELF_TYPE) {
        set_type(SELF_TYPE);
    } else if (!type_exists(type_name)) {
        set_type(No_type);
        curr_table->semant_error(curr_file, this) << "new: type undefined\n";
    } else {
        set_type(type_name);
    }
    return type;
}

Symbol isvoid_class::recurse() {
    e1->recurse();
    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::recurse() {
    set_type(No_type);
    return No_type;
}

Symbol object_class::recurse() {
    if (name == self) {
        set_type(SELF_TYPE);
        return SELF_TYPE;
    }
    auto id_type = id_exists(name);
    if (id_type == No_type) {
        curr_table->semant_error(curr_file, this)
            << "object: identifier undefined\n";
    }
    set_type(id_type);
    return id_type;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    // Now class tree (inheritance graph) is built
    // Compilation will be halted if there is any class hierarchy error
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // Check if methods are of good types, if not, turn into No_type
    // Also check if there is inheritance from undefiend class
    classtable->check_types();

    classtable->recurse();

    if (!type_exists(Main)) {
        curr_table->semant_error() << "class Main must be defined\n";
    }
    /* some semantic analysis code may go here */

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    /*dump_with_types() is done in semant-phase.cc */
}
