#include "tiny-scope.h"

namespace Tiny
{

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  SymbolMapping *new_sc = new SymbolMapping (current_scope.empty() ? NULL : &scope ());
  current_scope.push_back (new_sc);
}

void
Scope::pop_scope ()
{
  gcc_assert (!current_scope.empty());
  current_scope.pop_back ();
}

Symbol *
Scope::query (const std::string &str)
{
  const SymbolMapping *sc = &this->scope();
  while (sc != NULL)
    {
      if (Symbol *sym = sc->get (str))
	{
	  return sym;
	}
      sc = sc->get_enclosing();
    }
  return NULL;
}
}
