
# Функція для фільтрування в мережі
create_graph <- function(edges, vertices) {
  
  edges <- edges %>% 
    filter(person_x %in% vertices$person,  # person_x
           person_y %in% vertices$person)  # person_y
  
  v_in_edges <- edges %>% 
    gather(key = vert_type, value = vertice, 
           person_x:person_y) %>% 
    select(vertice) %>% 
    distinct() %>% 
    pull()
  
  vertices <- vertices %>% 
    filter(person %in% v_in_edges)
  
  g <- igraph::graph_from_data_frame(edges, vertices = vertices)
  g
}

# Network №1 ####
mps_force_network <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "factions", index = V(g)))
  
  network$nodes$size <- vertex_attr(g, "weight_name")
  
  # https://rdrr.io/cran/networkD3/src/inst/examples/examples.R
  
  #MyClickScript <- 'alert("You clicked " + d.name + " which is in row " + (d.index + 1) + " of your original R data frame");'
  MyClickScript = 'Shiny.onInputChange("d.index", d.name)'
  
  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               clickAction = MyClickScript,
               Source = "source", 
               Target = "target", 
               NodeID = "name", 

               Group = "group",
               Nodesize = "size",
               legend = TRUE, 
               zoom=TRUE,
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}





# Network №2 ####
mps_force_network_hidden_groups <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "hidden_groups", index = V(g)))
  
  network$nodes$size <- vertex_attr(g, "weight_name")
  

  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               
               # colourScale = scalecolors(vertices, 'YlOrRd'),
               
               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               Nodesize = "size",
               legend = TRUE,  #clickAction = 'Shiny.onInputChange("id", d.name)',
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}

# Network №3 #### 
mps_force_network_komitet <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "department_k", index = V(g)))
  

  
  network$nodes$size <- vertex_attr(g, "weight_name")
  

  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes,
               
               #Nodesize = weight_name,
               
               Nodesize = "size",
               
               # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"), 
               
               linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
               radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"), 
  
               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               
               legend = TRUE, 
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}



