
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __javax_swing_text_DefaultStyledDocument$StyleChangeListener__
#define __javax_swing_text_DefaultStyledDocument$StyleChangeListener__

#pragma interface

#include <java/lang/Object.h>
extern "Java"
{
  namespace javax
  {
    namespace swing
    {
      namespace event
      {
          class ChangeEvent;
      }
      namespace text
      {
          class DefaultStyledDocument;
          class DefaultStyledDocument$StyleChangeListener;
      }
    }
  }
}

class javax::swing::text::DefaultStyledDocument$StyleChangeListener : public ::java::lang::Object
{

  DefaultStyledDocument$StyleChangeListener(::javax::swing::text::DefaultStyledDocument *);
public:
  virtual void stateChanged(::javax::swing::event::ChangeEvent *);
public: // actually package-private
  DefaultStyledDocument$StyleChangeListener(::javax::swing::text::DefaultStyledDocument *, ::javax::swing::text::DefaultStyledDocument$StyleChangeListener *);
  ::javax::swing::text::DefaultStyledDocument * __attribute__((aligned(__alignof__( ::java::lang::Object)))) this$0;
public:
  static ::java::lang::Class class$;
};

#endif // __javax_swing_text_DefaultStyledDocument$StyleChangeListener__
