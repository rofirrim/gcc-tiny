#ifndef TINY_SYMBOL_MAPPING_H
#define TINY_SYMBOL_MAPPING_H

#include "tiny/tiny-symbol.h"
#include <map>

namespace Tiny
{

struct SymbolMapping
{
public:
  SymbolMapping (SymbolMapping *enclosing_, int scope_id_)
    : enclosing (enclosing_), scope_id (scope_id_)
  {
  }

  void insert (Symbol *s);
  Symbol *query_in_scope (const std::string &str);
  Symbol *query (const std::string &str);

private:
  SymbolMapping *enclosing;
  int scope_id;

  typedef std::map<std::string, Symbol *> Map;
  Map map;
};

}

#endif // TINY_SYMBOL_MAPPING_H
