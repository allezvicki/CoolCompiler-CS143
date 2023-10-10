#ifndef SEMANT_H_
#define SEMANT_H_

#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "list.h"
#include "stringtab.h"
#include "symtab.h"
#include <assert.h>
#include <iostream>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#define TRUE 1
#define FALSE 0

// Error type, thrown when semantic error detectecd
struct CoolError {
    tree_node *node{};
    char *msg{};

    CoolError(tree_node *_node, char *_msg) : node(_node), msg(_msg) {}
};

// This is node for the class inheritance tree. ClassNode is
// essentially a wrapper around the AST _Class tree node.
class ClassNode {
  private:
    Class_ _class;
    // TODO: Why did I use List??? lol... I could just use vector...
    List<ClassNode> *_children;
    ClassNode *_parent;
    int _height{};

    // Newly defined(or overriding) method on this node
    std::vector<Method> _methods;
    // Newly defined attributes
    std::vector<Symbol> _attrs;

  public:
    explicit ClassNode(Class_ clss, ClassNode *parent = NULL)
        : _class(clss), _children(NULL), _parent(parent) {
        // For Int, Bool...
        if (parent != NULL) {
            _height = 1;
        }
    }

    // add a child to parent
    void add_child(ClassNode *child);

    // check features: add methods and check if attributes are
    // redefined
    //
    // May throw exception (method type error; attribute error)
    void check_features(std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
                        std::unordered_set<Symbol> &attrs,
                        std::vector<Method> &new_mtds,
                        std::vector<Symbol> &new_attrs);

    // build a tree using edges in the map
    //
    // May throw exception
    void build(std::unordered_map<Symbol, std::vector<class__class *>> &,
               std::unordered_set<Symbol> &, int &,
               std::unordered_map<Symbol, std::vector<Symbol>> &mtds,
               std::unordered_set<Symbol> &attrs);

    // Checks if node has a specific ancestor
    bool has_ancestor(ClassNode *);

    // Get least common ancestor
    Symbol get_lca(ClassNode *);

    // Get method for the class, returns NULL if not found
    Method *get_method(Symbol method_name);
    // Recursive descent on class tree
    void recurse();

    // Check method types are valid
    void check_types();
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable;
typedef ClassTable *ClassTableP;

class ClassTable {
  private:
    int semant_errors;
    ClassNode *install_basic_classes();
    ostream &error_stream;
    ClassNode *_class_root{};

  public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream &semant_error();
    ostream &semant_error(Class_ c);
    ostream &semant_error(Symbol filename, tree_node *t);
    void recurse();
    void check_types();
};

#endif
