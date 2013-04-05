type color = int * int * int;
type xy = int * int;
type image = color ref array array;

fun image ((x,y) : xy) ((r,g,b) : color) = Array.tabulate(x, fn _ => Array.tabulate(y, fn _ => ref((r,g,b)))) : image;

fun size (image : image) = (Array.length(image), Array.length(Array.sub(image,0))) : xy;

fun drawPixel (image : image) (color : color) ((x,y) : xy) = Array.update(Array.sub(image,x), y, ref(color));

fun colorToString ((r,g,b) : color) = Int.toString(r) ^ " " ^ Int.toString(g) ^ " " ^ Int.toString(b) ^ " ";

fun toPPM image filename =
  let
    val oc = TextIO.openOut filename
    val (w,h) = size image
    val i = ref(~1)
    val j = ref(~1)
    val acc = ref("")
  in
    TextIO.output(oc, "P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
    while(i := !i + 1; !i < h) do (
      while(j := !j + 1; !j < w) do (
        acc := !acc ^ colorToString(!(Array.sub(Array.sub(image, !j), !i)))
      );
      TextIO.output(oc, !acc ^ "\n");
      j := ~1;
      acc := ""
    );
    TextIO.closeOut oc
  end;

fun drawAll (coloringFunction) (image : image) = 
  let
    val i = ref(~1)
    val j = ref(~1)
    val (w,h) = size image
  in
    while(i := !i + 1; !i < w) do (
      while(j := !j + 1; !j < h) do (
        (* Array.update(Array.sub(image, !i), !j, ref (coloringFunction ((!i,!j) : xy))) *)
        Array.sub(Array.sub(image, !i), !j) := coloringFunction ((!i,!j) : xy)
        (* print ("i: " ^ Int.toString(!i) ^ " j: " ^ Int.toString(!j) ^ " clr: " ^ colorToString (coloringFunction ((!i,!j) : xy))) *)
      );
      j := ~1
    )
  end;

fun gradient ((x,y) : xy) : color = (((x div 30) * 30) mod 256, 0, ((y div 30) * 30) mod 256);

fun gradImage () =
  let
    val img = image (640,480) (0,0,0)
  in
    drawAll (gradient) (img);
    toPPM img "/root/gradient1337.ppm"
  end;

fun mandelbrot maxIter (x,y) =
  let
    fun solve (a,b) c =
      if c = maxIter then
        1.0
      else
        if (a*a + b*b <= 4.0) then
          solve (a*a - b*b + x,2.0*a*b + y) (c+1)
      else
        (real c)/(real maxIter)
  in
    solve (x,y) 0
  end;

fun chooseColour n =
  let
    val r = round ((Math.cos n) * 255.0)
    val g = round ((Math.cos n) * 255.0)
    val b = round ((Math.sin n) * 255.0)
  in
    (r,g,b) : color
  end;

fun reScale ((w,h) : xy) ((cx, cy, s) : (real*real*real)) ((x,y) : xy) =
  let
    val p = s*((real(x)/real(w))-0.5) + cx
    val q = s*((real(y)/real(h))-0.5) + cy
  in
    (p, q)
  end;

fun compute ((cx, cy, s) : (real*real*real)) =
  let
    val img = image (640,480) (0,0,0)
    val (w,h) = size img
    fun cfunction (x,y) = chooseColour (mandelbrot (100) (reScale (w,h) (cx,cy,s) (x,y)))
  in
    drawAll cfunction img;
    toPPM img "/root/testM.ppm"
  end;

(* Compute part of the set *)
compute (~0.74364990, 0.13188204, 0.00073801);