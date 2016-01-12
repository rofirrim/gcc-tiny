#ifndef TINY_SYMBOL_MAPPING_H
#define TINY_SYMBOL_MAPPING_H

#include "tiny/tiny-symbol.h"
#include <map>

namespace Tiny
{

struct SymbolMapping
{
public:
  SymbolMapping (SymbolMapping *enclosing_)
    : enclosing (enclosing_)
  {
  }

  void insert (Symbol *s);
  Symbol *get (const std::string &str) const;

  SymbolMapping* get_enclosing() const
    {
      return enclosing;
    }

private:
  SymbolMapping *enclosing;

  typedef std::map<std::string, Symbol *> Map;
  Map map;
};

}

#endif // TINY_SYMBOL_MAPPING_H
