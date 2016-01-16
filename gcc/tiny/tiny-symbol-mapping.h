#ifndef TINY_SYMBOL_MAPPING_H
#define TINY_SYMBOL_MAPPING_H

#include "tiny/tiny-symbol.h"
#include <tr1/memory>
#include <map>

namespace Tiny
{

struct SymbolMapping
{
public:

  void insert (SymbolPtr s);
  SymbolPtr get (const std::string &str) const;

private:

  typedef std::map<std::string, SymbolPtr > Map;
  Map map;
};

}

#endif // TINY_SYMBOL_MAPPING_H
