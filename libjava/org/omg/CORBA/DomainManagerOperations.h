
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __org_omg_CORBA_DomainManagerOperations__
#define __org_omg_CORBA_DomainManagerOperations__

#pragma interface

#include <java/lang/Object.h>
extern "Java"
{
  namespace org
  {
    namespace omg
    {
      namespace CORBA
      {
          class DomainManagerOperations;
          class Policy;
      }
    }
  }
}

class org::omg::CORBA::DomainManagerOperations : public ::java::lang::Object
{

public:
  virtual ::org::omg::CORBA::Policy * get_domain_policy(jint) = 0;
  static ::java::lang::Class class$;
} __attribute__ ((java_interface));

#endif // __org_omg_CORBA_DomainManagerOperations__
