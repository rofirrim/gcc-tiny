#include "tiny-scope.h"

namespace Tiny
{

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  map_stack.push_back (SymbolMapping());
}

void
Scope::pop_scope ()
{
  gcc_assert (!map_stack.empty());
  map_stack.pop_back ();
}

SymbolPtr
Scope::lookup (const std::string &str)
{
  for (MapStack::reverse_iterator map = map_stack.rbegin ();
       map != map_stack.rend (); map++)
    {
      if (SymbolPtr sym = map->get (str))
	{
	  return sym;
	}
    }
  return SymbolPtr();
}
}
