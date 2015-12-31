
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __java_awt_image_DirectColorModel__
#define __java_awt_image_DirectColorModel__

#pragma interface

#include <java/awt/image/PackedColorModel.h>
#include <gcj/array.h>

extern "Java"
{
  namespace java
  {
    namespace awt
    {
      namespace color
      {
          class ColorSpace;
      }
      namespace image
      {
          class ColorModel;
          class DirectColorModel;
          class Raster;
          class WritableRaster;
      }
    }
  }
}

class java::awt::image::DirectColorModel : public ::java::awt::image::PackedColorModel
{

public:
  DirectColorModel(jint, jint, jint, jint);
  DirectColorModel(jint, jint, jint, jint, jint);
  DirectColorModel(::java::awt::color::ColorSpace *, jint, jint, jint, jint, jint, jboolean, jint);
  virtual jint getRedMask();
  virtual jint getGreenMask();
  virtual jint getBlueMask();
  virtual jint getAlphaMask();
  virtual jint getRed(jint);
  virtual jint getGreen(jint);
  virtual jint getBlue(jint);
  virtual jint getAlpha(jint);
private:
  jint extractAndNormalizeSample(jint, jint);
  jint extractAndScaleSample(jint, jint);
public:
  virtual jint getRGB(jint);
  virtual jint getRed(::java::lang::Object *);
  virtual jint getGreen(::java::lang::Object *);
  virtual jint getBlue(::java::lang::Object *);
  virtual jint getAlpha(::java::lang::Object *);
  virtual jint getRGB(::java::lang::Object *);
  virtual ::java::lang::Object * getDataElements(jint, ::java::lang::Object *);
private:
  jint valueToField(jint, jint, jint);
  jint value16ToField(jint, jint);
public:
  virtual JArray< jint > * getComponents(jint, JArray< jint > *, jint);
  virtual JArray< jint > * getComponents(::java::lang::Object *, JArray< jint > *, jint);
  virtual ::java::awt::image::WritableRaster * createCompatibleWritableRaster(jint, jint);
  virtual jint getDataElement(JArray< jint > *, jint);
  virtual ::java::lang::Object * getDataElements(JArray< jint > *, jint, ::java::lang::Object *);
  virtual ::java::awt::image::ColorModel * coerceData(::java::awt::image::WritableRaster *, jboolean);
  virtual jboolean isCompatibleRaster(::java::awt::image::Raster *);
public: // actually package-private
  virtual ::java::lang::String * stringParam();
public:
  virtual ::java::lang::String * toString();
  static ::java::lang::Class class$;
};

#endif // __java_awt_image_DirectColorModel__