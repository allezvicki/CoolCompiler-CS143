#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

// This is node for the class inheritance tree. ClassNode is
// essentially a wrapper around the AST _Class tree node.
class ClassNode {
private:
    Class_ _class;
    List<ClassNode>* _children;

public:
    explicit ClassNode(Class_ clss) : _class(clss), _children(NULL) {}

    // add a child to parent
    void add_child(ClassNode* child);

    // build a tree using edges in the map
    bool build(std::unordered_map<Symbol, std::vector<class__class*>>&, std::unordered_set<Symbol>&, int&);

    /* // Returns true is cycle detected */
    /* bool check_cycle(); */

    /* // Returns true if already visited */
    /* bool visit(); */
};

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  ClassNode* install_basic_classes();
  ostream& error_stream;
  ClassNode* _class_root {};

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};



#endif

