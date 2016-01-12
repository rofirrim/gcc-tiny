#ifndef TINY_SCOPE_H
#define TINY_SCOPE_H

#include "tiny-symbol-mapping.h"
#include <vector>

namespace Tiny
{

struct Scope
{
public:
  SymbolMapping &
  scope ()
  {
    return *(current_scope.back ());
  }

  SymbolMapping &
  get_top_level_scope ()
  {
    gcc_assert (!current_scope.empty ());
    return *(current_scope.front ());
  }

  void push_scope ();
  void pop_scope ();

  Scope ();

private:
  std::vector<SymbolMapping *> current_scope;
  static int scope_id;
};

}

#endif // TINY_SCOPE_H
