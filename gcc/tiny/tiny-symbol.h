#ifndef TINY_SYMBOL_H
#define TINY_SYMBOL_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

namespace Tiny
{

struct Symbol
{
public:
  Symbol (const std::string &name_) : name (name_), decl (error_mark_node)
  {
    gcc_assert (name.size () > 0);
  }

  std::string
  get_name () const
  {
    return name;
  }

  std::string
  get_global_name () const
  {
    gcc_assert (global_name.size () > 0);
    return global_name;
  }

  void
  set_global_name (const std::string &global_name_)
  {
    global_name = global_name_;
  }

  void
  set_tree_decl (tree decl_)
  {
    gcc_assert (TREE_CODE (decl_) == VAR_DECL);
    decl = decl_;
  }

  tree
  get_tree_decl () const
  {
    return decl;
  }

private:
  std::string name;
  std::string global_name;
  tree decl;
};
}

#endif // TINY_SYMBOL_H
