type quadtree_node = NoPoint
                   | Point of int * int
                   | QNode of quadtree_node (* bottom left *)
                           * quadtree_node (* top left *)
                           * quadtree_node (* bottom right *)
                           * quadtree_node (* top right *)
type quadtree = { width:int; height:int; root:quadtree_node }

(* print a graphical representation (svg) of a quadtree (2. argument) to a file (1. argument) *)
let print_quadtree filename qtree =
  let file = open_out filename in
  let rec impl (x1, y1, x2, y2) = function NoPoint -> ()
    | Point (x,y) -> Printf.fprintf file "<circle cx=\"%d\" cy=\"%d\" r=\"1\" fill=\"black\"/>\n" x (qtree.height - y)
    | QNode (nn, np, pn, pp) ->
      let xmid = (x1 + x2) / 2 in
      let ymid = (y1 + y2) / 2 in
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n"
        x1 (qtree.height-ymid) x2 (qtree.height-ymid);
      Printf.fprintf file "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n"
        xmid (qtree.height -y1) xmid (qtree.height-y2);
      impl (x1, y1, xmid, ymid) nn;
      impl (x1, ymid, xmid, y2) np;
      impl (xmid, y1, x2, ymid) pn;
      impl (xmid, ymid, x2, y2) pp
  in
  Printf.fprintf file "<?xml version=\"1.0\" standalone=\"no\"?>\n
    <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n
    <svg viewBox = \"0 0 %d %d\">\n
    <rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"white\"/>\n" qtree.width qtree.height
    qtree.width qtree.height;
  impl (0, 0, qtree.width, qtree.height) qtree.root;
  Printf.fprintf file "</svg>";
  close_out file
  

(* ONLY this function was our task, the rest was given *)
let insert (px,py) qtree = 
    let rec helper (x1, y1, x2, y2) (px,py) = 
    let xmid = (x1 + x2) / 2 in
    let ymid = (y1 + y2) / 2 in
    function 
    | NoPoint -> Point (px, py) 
    | QNode (bl, tl, br, tr) -> ( match px < xmid, py < ymid with 
            | true, true -> QNode( helper (x1,y1,xmid,ymid) (px,py) bl, tl, br, tr)
            | true, false -> QNode( bl, helper (x1,ymid,xmid,y2) (px,py) tl, br, tr)
            | false, true -> QNode( bl, tl, helper (xmid,y1,x2,ymid) (px,py) br, tr)
            | false, false -> QNode( bl, tl, br, helper (xmid,ymid,x2,y2) (px,py) tr)
            )
    | Point (px', py') -> if (px',py') = (px,py) then Point(px',py') else
    helper (x1,y1,x2,y2) (px,py) (helper (x1,y1,x2,y2) (px',py') (QNode(NoPoint,NoPoint,NoPoint,NoPoint)) )
    in {qtree with root = helper (0, 0, qtree.width, qtree.height) (px,py) qtree.root} 
