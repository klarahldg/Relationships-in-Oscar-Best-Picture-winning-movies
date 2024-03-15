library(igraph)
library(tidyr)
library(dplyr)
library(ggplot2)

file.choose()
data <- read.csv ("C:\\Users\\User\\Documents\\uni\\dh\\projektarbeit\\datasets\\bestpicture.csv", 
                  header = TRUE, encoding = "UTF-8") 




# data exploren
dim(data)
names(data)
head(data)
summary(data)
str(data)



# column/matrix for co-occurrence network
genre_data <- separate_rows(data, Movie.Genre, sep = ",")
genre_data <- genre_data[, c("Film", "Movie.Genre")]

genre_matrix <- matrix(0, 
                       ncol = length(unique(genre_data$Movie.Genre)),
                       nrow = length(unique(genre_data$Film)),
                       dimnames = list(unique(genre_data$Film), unique(genre_data$Movie.Genre)))

# filling matrix
for (i in 1:nrow(genre_data)) {
  genres <- unlist(strsplit(genre_data$Movie.Genre[i], ", "))
  film <- genre_data$Film[i]
  genre_matrix[film, genres] <- 1
}

co_occurrence_matrix <- t(genre_matrix) %*% genre_matrix
diag(co_occurrence_matrix) <- 0




# make edgelists
g_edges <- which(co_occurrence_matrix > 0, arr.ind = TRUE)
g_edgelist <- cbind(rownames(co_occurrence_matrix)[edges[, 1]], colnames(co_occurrence_matrix)[g_edges[, 2]])



# create / plot 
genre_graph <- graph_from_edgelist(as.matrix(g_edgelist), directed = FALSE)
plot(genre_graph, vertex.size = 10, vertex.label.dist = 1.5, vertex.label.cex = 0.7, edge.arrow.size = 0.1)



# node weight
g_node_degrees <- degree(genre_graph)
V(genre_graph)$weight <- g_node_degrees



# plot schöner machen 
plot(
  genre_graph,
  layout = layout_with_fr(genre_graph, niter = 1000, area = 1000),  
  vertex.size = V(genre_graph)$weight * 0.8,  
  vertex.label.dist = 1.6,  
  vertex.label.cex = 0.9,  
  vertex.label.color = "black", 
  vertex.color =  "darkolivegreen2", 
  vertex.frame.color = "darkgreen",  
  edge.color = "darkolivegreen3",  
  edge.width = 0.6,  
  edge.arrow.size = 0.5,  
  main = "Co-occurrence Network of Genres 
  Based on Best Picture Movies", 
)


# export for gephi
write.csv(as.data.frame(get.edgelist(genre_graph)), "C:\\Users\\User\\Documents\\uni\\dh\\projektarbeit\\plots\\genre_graph_edges.csv", row.names = FALSE)



# network analysis
density <- graph.density(genre_graph)
print(paste("Network density:", format(density, digits = 4)))


sport_thriller_path <- shortest_paths(
  graph = genre_graph,
  from = "Sport",
  to = "Thriller"
)
print(sport_thriller_path$vpath)
print(paste("Path length: ", length(sport_thriller_path$vpath[[1]])-1))


diameter <- diameter(genre_graph)
print(paste("Network diamter: ", diameter))
components <- components(genre_graph)
print(paste("Network components: ", components$no))
dg <- decompose.graph(genre_graph)
subgraph <- dg[[1]]
diameter <- diameter(subgraph)
print(paste("Diameter of subgraph: ", diameter))


transitivity <- transitivity(genre_graph)
print(paste("Transistivity of graph: ", transitivity))


degree.genre_graph <- degree(genre_graph)
head(degree.genre_graph)
genre_graph <- set_vertex_attr(genre_graph, "degree", index = V(genre_graph), degree.genre_graph)
print(paste("Largest degree: ", sort(degree.genre_graph, decreasing = T) [1]))
sort(degree.genre_graph, decreasing = T) [1:20]





# create / plot degree centrality

degree_data <- data.frame(
  Genre = names(degree.genre_graph),
  Degree = as.numeric(degree.genre_graph)
)
degree_data <- degree_data[order(-degree_data$Degree), ]

ggplot(degree_data[1:21, ], aes(x = reorder(Genre, Degree), y = Degree)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Genre", y = "Degree Centrality", title = "Best Picture Genres by Degree Centrality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# filter/count for winning/loosing movies graphs
winning_genres <- data[data$Award == "Winner", ]
winning_genres <- separate_rows(winning_genres, Movie.Genre, sep = ",")
winning_count <- table(winning_genres$Movie.Genre)
print(winning_count)

losing_genres <- data[data$Award == "Nominee", ]
losing_genres <- separate_rows(losing_genres, Movie.Genre, sep = ",")
losing_count <- table(losing_genres$Movie.Genre)
print(losing_count)


# plot data frame as bar plot
winning_count_df <- as.data.frame(winning_count)
colnames(winning_count_df) <- c("Genre", "Count")
winning_count_df <- winning_count_df[order(-winning_count_df$Count), ]

ggplot(winning_count_df, aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen3") +
  labs(title = "Genre Distribution of Winning Movies", x = "Genre", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


losing_count_df <- as.data.frame(losing_count)
colnames(losing_count_df) <- c("Genre", "Count")
losing_count_df <- losing_count_df[order(-losing_count_df$Count), ]

ggplot(losing_count_df, aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen3") +
  labs(title = "Genre Distribution of Losing Movies", x = "Genre", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# both together nowww
all_genres <- unique(c(names(winning_count), names(losing_count)))
winning_count[setdiff(all_genres, names(winning_count))] <- 0
losing_count[setdiff(all_genres, names(losing_count))] <- 0

# data framess
winning_df <- data.frame(Genre = names(winning_count), Count = as.vector(winning_count), Award = "Winner")
losing_df <- data.frame(Genre = names(losing_count), Count = as.vector(losing_count), Award = "Nominee")
combined_counts <- bind_rows(winning_df, losing_df)


# plotting~
ggplot(combined_counts, aes(x = reorder(Genre, Count), y = Count, fill = Award)) +
  geom_bar(stat = "identity") +
  labs(title = "Genre Distribution of Winning and Losing Movies",
       x = "Genre", y = "Count", fill = "Award") +
  scale_fill_manual(values = c("Winner" = "darkolivegreen3", "Nominee" = "lightsalmon")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plotting without drama
combined_counts_filtered <- combined_counts[combined_counts$Genre != "Drama", ]

ggplot(combined_counts_filtered, aes(x = reorder(Genre, Count), y = Count, fill = Award)) +
  geom_bar(stat = "identity") +
  labs(title = "Genre Distribution of Winning and Losing Movies (Excluding Drama)",
       x = "Genre", y = "Count", fill = "Award") +
  scale_fill_manual(values = c("Winner" = "darkolivegreen3", "Nominee" = "lightsalmon")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
