#include "tiny-context.h"

namespace Tiny
{

int Context::scope_id = 0;

Context::Context ()
{
}

void
Context::push_scope ()
{
  Scope *new_sc = new Scope (current_scope.empty() ? NULL : &scope (), scope_id);
  current_scope.push_back (new_sc);
  scope_id++;
}

void
Context::pop_scope ()
{
  gcc_assert (!current_scope.empty());
  current_scope.pop_back ();
}
}
