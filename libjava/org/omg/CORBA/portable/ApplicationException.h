
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __org_omg_CORBA_portable_ApplicationException__
#define __org_omg_CORBA_portable_ApplicationException__

#pragma interface

#include <java/lang/Exception.h>
extern "Java"
{
  namespace org
  {
    namespace omg
    {
      namespace CORBA
      {
        namespace portable
        {
            class ApplicationException;
            class InputStream;
        }
      }
    }
  }
}

class org::omg::CORBA::portable::ApplicationException : public ::java::lang::Exception
{

public:
  ApplicationException(::java::lang::String *, ::org::omg::CORBA::portable::InputStream *);
  virtual ::java::lang::String * getId();
  virtual ::org::omg::CORBA::portable::InputStream * getInputStream();
private:
  static const jlong serialVersionUID = -2088103024111528125LL;
  ::org::omg::CORBA::portable::InputStream * __attribute__((aligned(__alignof__( ::java::lang::Exception)))) m_input;
  ::java::lang::String * m_id;
public:
  static ::java::lang::Class class$;
};

#endif // __org_omg_CORBA_portable_ApplicationException__
