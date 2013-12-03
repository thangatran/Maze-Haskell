import Data.String.Utils
import Graphics.GD

--Node r c
data Cell = Node Int Int

update_level_2 (head:tail) c index_c
		| index_c < c = (head : update_level_2 tail c (index_c+1))
		| index_c == c = (True:tail)
		| otherwise = error "Bad index"

update_level_1 (head:tail) r c index_r
		| index_r < r = (head : update_level_1 tail r c (index_r+1))
		| index_r == r = ((update_level_2 head c 0):tail)
		| otherwise = error "Bad index"

mark m_lst r c = update_level_1 m_lst r c 0


--initialize m_lst
init_m_c c = if c == 0 then [] else (False:init_m_c (c-1))
init_m_r r c = if r == 0 then [] else ((init_m_c c) : init_m_r (r-1) c)

init_m_lst maze = init_m_r h w
		  where h = length maze
		  	w = length (head maze)

findNeighbors (Node r c) m_lst maze = let (top, m_lst1, b_lst1) =
						if r-1 >= 0 && ((maze!!(r-1))!!c) /= 'x'
							    && (m_lst!!(r-1))!!c == False
						then ([(Node (r-1) c)], mark m_lst (r-1) c, [(r-1,c,r,c)]) else ([], m_lst,[])
			              in let (right, m_lst2, b_lst2) = 
						if c+1 < w && ((maze!!r)!!(c+1)) /= 'x'
							   && (m_lst!!r)!!(c+1) == False
						then ([(Node r (c+1))], mark m_lst1 r (c+1),[(r,c+1,r,c)]) else ([], m_lst1,[])
				      in let (bottom, m_lst3, b_lst3) = 
						if r+1 < h && ((maze!!(r+1))!!c) /= 'x'
							   && (m_lst!!(r+1))!!c == False
						then ([(Node (r+1) c)], mark m_lst2 (r+1) c,[(r+1,c,r,c)]) else ([], m_lst2,[])
				      in let (left, m_lst4, b_lst4) = 
						if c-1 >=0 && ((maze!!r)!!(c-1)) /= 'x'
							   && (m_lst!!r)!!(c-1) == False
						then ([(Node r (c-1))], mark m_lst3 r (c-1),[(r,c-1,r,c)]) else ([], m_lst3,[])
				      in (top ++ right ++ bottom ++ left, m_lst4, b_lst1 ++ b_lst2 ++ b_lst3 ++ b_lst4)
				      where h = length maze
					    w = length (head maze)


deque [] _ b_lst _ = b_lst

deque (s:tail) m_lst b_lst maze = let (neighbors,m_lst1,b_lst1) = findNeighbors s m_lst maze
		                  in deque (tail ++ neighbors) m_lst1 (b_lst ++ b_lst1) maze

--BFS current_cell maze mark_array backtrack_array hero_cell exit_cell
--    return the path from hero position to exit door
bfs h_cell maze = return (let q = [h_cell]
			  in let m_lst = init_m_lst maze
			  in let b_lst = deque q m_lst [] maze
			  in b_lst)

icon_l = 40

draw_cell r c maze_img maze = do icon <- io_icon
	 		         copyRegion src_up_left (icon_l,icon_l) icon des_up_left maze_img
				 return (if cell_char == 's' then ((r,c),(-1,-1))
					 else if cell_char == 'e' then ((-1,-1),(r,c))
					      else ((-1,-1),(-1,-1)))
			      where src_up_left = (0,0)
			            des_up_left = (c*icon_l,r*icon_l)
				    cell_char = ((maze !! r) !! c)
				    io_icon = case cell_char of
						'x' -> loadPngFile "./images/wall-icon.png"
						's' -> loadPngFile "./images/hero-icon.png"
						'e' -> loadPngFile "./images/exit-icon.png"
						otherwise -> loadPngFile "./images/floor-icon.png"

choose_cell cell new_cell = if new_cell == (-1,-1) then cell else new_cell  

draw_maze r c maze_img maze h_cell x_cell = do (h_cell1,x_cell1) <- draw_cell r c maze_img maze
                                 	       if (c+1) >= w && (r+1) < h then draw_maze (r+1) 0 maze_img maze
											 (choose_cell h_cell h_cell1)
											 (choose_cell x_cell x_cell1)
				               else if (c+1) < w then draw_maze r (c+1) maze_img maze
										(choose_cell h_cell h_cell1)
										(choose_cell x_cell x_cell1)
				                    else return (h_cell, x_cell)
			      		    where h = length maze
				                  w = length (head maze)


look_up [] _ _ = (-1,-1)
look_up ((src_r,src_c,dest_r,dest_c):tail) x_cell_r x_cell_c = if src_r == x_cell_r && src_c == x_cell_c then (dest_r,dest_c)
								 else look_up tail x_cell_r x_cell_c

trace_back path_lst h_cell_r h_cell_c x_cell_r x_cell_c = do --print path_lst
							     print x_cell_r
							     print x_cell_c
							     print "----"
							     if h_cell_r == x_cell_r && h_cell_c == x_cell_c then return ()
						             else let (cell_r, cell_c) = look_up path_lst x_cell_r x_cell_c
								  in trace_back path_lst h_cell_r h_cell_c cell_r cell_c

draw_path maze (h_cell_r,h_cell_c) (x_cell_r,x_cell_c) = do path_lst <- bfs (Node h_cell_r h_cell_c) maze
							    trace_back path_lst h_cell_r h_cell_c x_cell_r x_cell_c
							    return ()

parseMazeFromString file_str = do return $split "," $replace "\n" "" file_str

main = do file_str <- readFile "./test.txt"
	  maze <- parseMazeFromString file_str
	  maze_img <- newImage ((length (head maze))*icon_l,(length maze)*icon_l)
	  (h_cell, x_cell) <- draw_maze 0 0 maze_img maze (-1,-1) (-1,-1)
	  draw_path maze h_cell x_cell
	  savePngFile "./result.png" maze_img
