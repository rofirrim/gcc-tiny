#include "tiny-scope.h"

namespace Tiny
{

int Scope::scope_id = 0;

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  SymbolMapping *new_sc = new SymbolMapping (current_scope.empty() ? NULL : &scope (), scope_id);
  current_scope.push_back (new_sc);
  scope_id++;
}

void
Scope::pop_scope ()
{
  gcc_assert (!current_scope.empty());
  current_scope.pop_back ();
}
}
