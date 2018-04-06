{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Based on WriteJPEG example from OpenGL24.de                       }
{    http://www.opengl24.de/header/libjpeg                             }
{                                                                      }
{    Further changes Copyright Creative IT.                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsJPEGEncoderOptions;

interface

type
   TJPEGOption = (
      jpgoOptimize,        // optimize Huffman tables
      jpgoNoJFIFHeader,    // don't write JFIF header
      jpgoProgressive      // progressive JPEG
      );
   TJPEGOptions = set of TJPEGOption;

implementation

end.
