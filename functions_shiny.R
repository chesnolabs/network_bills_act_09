

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


mps_force_network <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "factions", index = V(g)))
  
  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               

               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               #Nodesize = "size",
               legend = TRUE, 
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}


mps_force_network_hidden_groups <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "hidden_groups", index = V(g)))
  

  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               
               # colourScale = scalecolors(vertices, 'YlOrRd'),
               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               #Nodesize = "size",
               legend = TRUE,  #clickAction = 'Shiny.onInputChange("id", d.name)',
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}

mps_force_network_komitet <- function(edges, vertices) {
  
  g <- create_graph(edges, vertices)
  
  network <- igraph_to_networkD3(g, group = vertex_attr(g, "komitet", index = V(g)))

  forceNetwork(Links = network$links, 
               
               Nodes = network$nodes, 
               

               Source = "source", 
               Target = "target", 
               NodeID = "name", 
               Group = "group",
               #Nodesize = "size",
               legend = TRUE, 
               opacity = 0.9, fontSize = 30, 
               bounded = TRUE, charge = -15)
}
