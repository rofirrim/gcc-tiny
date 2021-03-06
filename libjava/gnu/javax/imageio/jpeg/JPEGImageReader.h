
// DO NOT EDIT THIS FILE - it is machine generated -*- c++ -*-

#ifndef __gnu_javax_imageio_jpeg_JPEGImageReader__
#define __gnu_javax_imageio_jpeg_JPEGImageReader__

#pragma interface

#include <javax/imageio/ImageReader.h>
extern "Java"
{
  namespace gnu
  {
    namespace javax
    {
      namespace imageio
      {
        namespace jpeg
        {
            class JPEGDecoder;
            class JPEGImageReader;
        }
      }
    }
  }
  namespace java
  {
    namespace awt
    {
      namespace image
      {
          class BufferedImage;
      }
    }
  }
  namespace javax
  {
    namespace imageio
    {
        class ImageReadParam;
      namespace metadata
      {
          class IIOMetadata;
      }
      namespace spi
      {
          class ImageReaderSpi;
      }
    }
  }
}

class gnu::javax::imageio::jpeg::JPEGImageReader : public ::javax::imageio::ImageReader
{

public: // actually protected
  JPEGImageReader(::javax::imageio::spi::ImageReaderSpi *);
public:
  virtual jint getHeight(jint);
  virtual ::javax::imageio::metadata::IIOMetadata * getImageMetadata(jint);
  virtual ::java::util::Iterator * getImageTypes(jint);
  virtual jint getNumImages(jboolean);
  virtual ::javax::imageio::metadata::IIOMetadata * getStreamMetadata();
  virtual jint getWidth(jint);
  virtual ::java::awt::image::BufferedImage * read(jint, ::javax::imageio::ImageReadParam *);
private:
  void checkIndex(jint);
  void checkStream();
  void decodeStream();
public: // actually package-private
  ::gnu::javax::imageio::jpeg::JPEGDecoder * __attribute__((aligned(__alignof__( ::javax::imageio::ImageReader)))) decoder;
public:
  static ::java::lang::Class class$;
};

#endif // __gnu_javax_imageio_jpeg_JPEGImageReader__
