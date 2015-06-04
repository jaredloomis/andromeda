This repository contains two APIs:
    - A lambda calculus-based library that generates GLSL code
    - A monadic eDSL that generates GLSL code

Comparison of the APIs
==

<h4>Functional version</h4>

```Haskell
vertexShader :: Expr (Vec3 Float -> Vec4 Float)
vertexShader = Lam $ (+-+ (1 :: Expr Float))

fragmentShader :: Expr (Vec4 Float)
fragmentShader = flt 1 +-+ flt 0 +-+ flt 0 +-+ fltd 1
```

Haskell GLSL eDSL
=================

A GLSL code generating DSL embedded in Haskell.

- Removes nearly all need for communication with OpenGL.
- Almost everything is type-checked at the Haskell type level, so many errors are caught at Haskell compile time, instead of OpenGL giving run-time errors.
- Complete support for multi-pass postprocessing using framebuffers / sampler2Ds.
- Uses GHC Type-Level literals to allow for easy communication from type-level to value-level. (This may change to use DataKinds.)
- Support for all OpenGL 4.0+ shader phases.
- Arrays and array indexing (length is checked at compile-time (This might have to change.)).
- vec swizzling and "vec concatenation".
- For loops.

--

<h4>Examples</h4>

The following Haskell code:

```Haskell
import Language.GLSL.Monad
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vec ((:.)(..), Vec3)

data Object = Object {
    objectVertices :: [Vec3 GL.GLfloat]
    }

main :: IO ()
main = print $ generateGLSL vertexShader

vertexShader :: ShaderM GL.VertexShader Object
vertexShader = do
    -- Use GLSL version 430 core.
    version "430 core"

    -- Declare an "in" GLSL variable corresponding to
    -- vertex position.
    position <- inn vec3 ("position", objectVertices)

    -- Set gl_Position to the given position.
    glPosition #= position +.+ fltd 1.0
```

generates the following GLSL shader code:

```GLSL
#version 430 core

in vec3 position;

void main()
{
    gl_Position = vec4(position, 1.0);
}
```

And this fragment shader:

```Haskell
fragmentShader :: ShaderM GL.VertexShader Object
fragmentShader = do
    version "430 core"

    -- Declare out vec4 color.
    color <- out vec4 "color"

    -- Swizzling example.
    color .@ X .& Y .& Z #= fltd 1 +.+ fltd 0 +.+ fltd 0
    color .@ W #= fltd 1
```

generates:

```GLSL
#version 430 core

out vec4 color;

void main()
{
    color.xyz = vec3(1, 0, 0);
    color.w = 1;
}
```

These two shaders can be combined to form a complete shader program:

```Haskell
shaderProgram :: IO (ShaderProgram Object)
shaderProgram =
    let shaderSequence = vertexShader -&> lastly fragentShader
    in createProgram shaderSequence exampleObject
```

Shader programs can be run very simply:

```Haskell
exampleObject = Object vertexPosition

vertexPosition :: [Vec3 GL.GLfloat]
vertexPosition =
    [(-1) :. (-1) :. 0 :. (),
        1 :. (-1) :. 0 :. (),
        0 :.  1   :. 0 :. ()]



main :: IO ()
main = do
    -- Open a window and initialize OpenGL.
    window <- openWindow
    initGL window

    program <- shaderProgram

    -- A "ShaderGalaxy" is all the information needed to
    -- draw a shader.
    let shaderGalaxy = PureGalaxy prog id exampleObject

    -- Enter main draw/update loop.
    mainLoop window $ [shaderGalaxy] -|> []
```

This will draw a red triangle on the screen.

--

TODO: more examples.

--

<h4>LICENSE</h4>
All source code in this repository is provided under the WTFPL Version 2.
```
        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
                    Version 2, December 2004 

 Copyright (C) 2004 Sam Hocevar <sam@hocevar.net> 

 Everyone is permitted to copy and distribute verbatim or modified 
 copies of this license document, and changing it is allowed as long 
 as the name is changed. 

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 

  0. You just DO WHAT THE FUCK YOU WANT TO.
```
