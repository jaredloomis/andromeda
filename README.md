Andromeda
==

This is a library that allows you to write and run GLSL shaders in Haskell.

Why would you want to write shaders in Haskell? Well with this eDSL instead of loading shaders, then creating and filling buffers with data, then binding each buffer, getting uniform locations and setting, etc., you just specify the data and how you want to transform and render the data.

More advantages of writing shaders in Haskell:
- Everything is typechecked at compile time (by GHC).
- Some of the distinction between GPU and CPU values / operations is removed.
- You can use normal Haskell functions when writing shaders.
- Optimizations can be performed on the shader.

Example
--

As an example of what the library can do, we will make a simple application that renders a red triangle to the screen.

First we have to import some stuff.

```Haskell
import Data.Vec ((:.)(..), Vec2, Vec3, Vec4)

import qualified Data.Vector.Storable as V

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.GLSL
import Andromeda.Simple.Var
import Andromeda.Simple.StdLib

import Andromeda.Simple.Render.Mesh
import Andromeda.Simple.Render.VertexBuffer
import Andromeda.Simple.Render.Compile
```

Then we define our triangle data.

```Haskell
triangle :: [Vec3 Float]
triangle = [(-1):.(-1):.0:.(),
              1 :.(-1):.0:.(),
              0 :.  1 :.0:.()]

myMesh :: Mesh
myMesh = Mesh [("vertex", MeshAttribute $ V.fromList triangle)] Triangles
```

Now we actually define the shaders. We'll start by defining what we want `gl_Position` to be.

```Haskell
glPosition :: Expr (Vec4 Float)
glPosition = fetch "vertex" (Vec3T SFloat) +-+ 1
```

`gl_Position` is a vec4 made from the vec3 "vertex" attribute we defined in the mesh and with a w value of 1. This is the same as `vec4(vertex, 1)` in GLSL.

Shaders are represented as a series of `Statement`s. In this case, out vertex shader is just setting `gl_Position`.

```Haskell
vertShader :: Statement
vertShader = AssignS "gl_Position" glPosition
```

For the frag shader, we just want to define a constant `out` value for the color, which will always be red.

```Haskell
outColor :: Expr (Vec3 Float)
outColor = flt 1 +-+ flt 0 +-+ flt 0

fragShader :: Statement
fragShader = OutS "color" outColor
```

And that's it! All we have to do now is use our shaders.

```Haskell
main :: IO ()
main = do
    win <- openWindow
    initGL win

    prog <- addMesh myMesh =<< compile vertShader fragShader

    mainLoop win prog
```

Run this program (found in `test/Main.hs`), and you should see a red triangle!

Note
--

This repository contains 3 APIs:
- A  new lambda calculus-based eDSL ("Simple")
- An old lambda calculus-based eDSL ("Lambda")
- A  Monad-based, imperative eDSL   ("Monad")

The above example uses the "Simple" API. I'm not working any more on the other versions, but they may be interesting or of some use, especially the imperative one.
