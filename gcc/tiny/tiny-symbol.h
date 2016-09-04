#ifndef TINY_SYMBOL_H
#define TINY_SYMBOL_H

#include "tiny/tiny-tree.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

#include <tr1/memory>

namespace Tiny
{

enum /* class */ SymbolKind
{
  INVALID,
  VARIABLE,
  TYPENAME
};

struct Symbol
{
public:
  Symbol (SymbolKind kind, const std::string &name_) : kind(kind), name (name_), decl (error_mark_node)
  {
    gcc_assert (name.size () > 0);
  }

  SymbolKind
  get_kind () const
  {
    return kind;
  }

  std::string
  get_name () const
  {
    return name;
  }

  void
  set_tree_decl (Tree decl_)
  {
    gcc_assert ((kind == VARIABLE && decl_.get_tree_code() == VAR_DECL)
                    || (kind == TYPENAME && decl_.get_tree_code() == TYPE_DECL));
    decl = decl_;
  }

  Tree
  get_tree_decl () const
  {
    return decl;
  }

private:
  SymbolKind kind;
  std::string name;
  Tree decl;
};

typedef std::tr1::shared_ptr<Symbol> SymbolPtr;
typedef std::tr1::shared_ptr<const Symbol> const_SymbolPtr;

}

#endif // TINY_SYMBOL_H
