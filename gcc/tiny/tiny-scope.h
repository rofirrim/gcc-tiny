#ifndef TINY_SCOPE_H
#define TINY_SCOPE_H

#include "tiny/tiny-symbol.h"
#include <map>

namespace Tiny
{

struct Scope
{
public:
  Scope (Scope *enclosing_, int scope_id_)
    : enclosing (enclosing_), scope_id (scope_id_)
  {
  }

  void insert (Symbol *s);
  Symbol *query_in_scope (const std::string &str);
  Symbol *query (const std::string &str);

private:
  Scope *enclosing;
  int scope_id;

  typedef std::map<std::string, Symbol *> Map;
  Map map;
};

}

#endif // TINY_SCOPE_H
