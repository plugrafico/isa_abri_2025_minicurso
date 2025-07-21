# script ISA ABRI students
library(tidyverse)
library(igraph)
library(ggraph)
library(quanteda)
library(stm)

# 1 TOY EXAMPLE ####

## Network ####

# node list
nl <- data.frame(
  name=letters[1:20]
)

# edge list with the citations
el <- data.frame(
  from=c(
    # a first crisp community a-c, q-t
    "a","t","b","b","a","q","q","r","t","c","b","c","a","c","q",  
    # a second less crisp d-i
    "d","d","d","e","e","f","f","f","g","g","h","i","i", 
    # a third isolated
    "l","m","o","n","o"
  ),
  to=c(
    "t","a","r","t","q","c","b","t","q","a","c","r","r","b","r",
    "i","h","k","d","k","i","g","r","d","t","k","k","p",
    "m","o","n","l","l"
  )
)

# form network from nodelist and edge list
g <- graph_from_data_frame(el, vertices=nl, directed=T)

## plot
ggraph(g)+
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(4, 'mm')) + 
  geom_node_point(color="grey", size=8)+ 
  scale_color_manual(values=c("grey"))+
  geom_node_text(aes(label=name), color="black", fontface="bold")+ 
  theme_void()+ 
  theme(plot.margin = margin(1, 1, 1, 1, "cm"), 
        legend.position = c(0.1,0.8))+
  guides(
    colour = "none" 
  ) 

## apply infomap algorithm to detect communities
set.seed(123)
g_im <- cluster_infomap(g)
print(communities(g_im)) # five communities 

## store as node-level attribute
V(g)$com <- membership(g_im)

## plot
p_g <- ggraph(g)+
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(4, 'mm')) + 
  geom_node_point(aes(shape=as.factor(com), color="grey"), size=8)+ 
  scale_color_manual(values=c("grey"))+
  geom_node_text(aes(label=name), color="black", fontface="bold")+
  scale_shape_manual(values=c("circle", "square", "diamond", "triangle", "asterisk"))+
  theme_void()+ 
  theme(plot.margin = margin(1, 1, 1, 1, "cm"), 
        legend.position = c(0.1,0.8))+
  guides(
    colour = "none",
    shape = guide_legend("Community")
  )
plot(p_g)

## Text ####
nl_at <- data.frame(
  name=letters[1:20],
  # topic at_rn is randomly uniformly distributed. It discriminates poorly between documents (ex: headers and formal elements)
  at_rn=c(0.3,0.2,0.3,0.2,0.1,0.1,0.2,0.3,0.1,0.1,0.2,0.2,0.3,0.1,0.2,0.1,0.1,0.3,0.2,0.3),
  # topic at_ce is a concentrated and exclusive topic. It discriminates strongly between documents. There are few documents about it and they focus exclusively on it
  at_ce=c(0.7,0.8,0.7,0.8,0.9,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0),
  # topic at_cs is concentrated but shared. A larger number of topics mention it, some intensily, others less.
  at_cs=c(0.0,0.0,0.0,0.0,0.0,0.9,0.8,0.7,0.9,0.9,0.8,0.8,0.7,0.4,0.4,0.3,0.0,0.2,0.3,0.5),
  # topic at_ev is more even. A small number of documents mention it, but the topics are not strongly concetrated
  at_ev=c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.5,0.4,0.6,0.9,0.5,0.5,0.2)
)
# for color, red = concentrated exclusive, green = concentrated shared, blue = even
nl_at$rgbsum <- rgb(nl_at$at_ce, nl_at$at_cs, nl_at$at_ev)

## a bar plot with each document's share in the four topics
nl_at_long <- nl_at %>%
  select(name, at_rn:at_ev) %>%
  pivot_longer(cols=at_rn:at_ev,
               names_to = "topic",
               values_to = "percentage")
nl_at_long$name <- factor(nl_at_long$name, levels=rev(sort(unique(nl_at_long$name))))

p_nl_at <- ggplot(nl_at_long, aes(y=as.factor(name), x=percentage))+
  geom_col(aes(fill=topic), position = position_stack(reverse = TRUE))+
  scale_fill_manual(values=c("red","green","blue","grey"),
                    labels=c("CE", "CS", "EV", "RN"))+
  labs(y="Document", x="Topic proportion", fill="Topic")+
  theme_minimal()+
  theme(
    axis.title = element_text(size = 16), # Axis titles font size
    axis.text = element_text(size = 14)
  )
p_nl_at

## Network with information about topics ####

# form a network from nodelist and edge list
g_at <- graph_from_data_frame(el, vertices=nl_at, directed=T)

## apply infomap algorithm to detect communities
set.seed(123)
g_at_im <- cluster_infomap(g_at, nb.trials = 50)

## store as node attribute
V(g_at)$com_tp <- membership(g_at_im)

## a plot with community as shapes and attributes as colors
p_g_at <- ggraph(g_at)+
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(4, 'mm')) + 
  geom_node_point(aes(color = rgbsum, shape=as.factor(com_tp)), size=8)+
  geom_node_text(aes(label=name), color="black", fontface="bold")+
  scale_color_identity()+
  scale_shape_manual(values=c("circle", "square", "diamond", "triangle", "asterisk"))+
  theme_void()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"), 
        legend.position = c(0.1,0.8))+
  guides(
    shape = guide_legend("Community")
  )+
  geom_point(aes(x=0.1,y=1.28,color="red"),size=4)+
  geom_point(aes(x=0.1,y=1.12,color="green"),size=4)+
  geom_point(aes(x=0.1,y=0.95,color="blue"),size=4)+
  geom_text(aes(x=0.01,y=1.2),
            label="Topic\n    CE\n    CS\n    EV",
            hjust=0,
            fontface="plain",
            lineheight=1.5)
plot(p_g_at)

## Calculate volume and concentration ####
# transform graph (with community information) into a df
df_at <- as_data_frame(g_at, what="vertices")

# calculate community topic volume
df_at_sum <- df_at |>
  # for each community
  group_by(com_tp) |>
  # sum topic percentages
  summarise(sumoftopics=across(at_rn:at_ev, sum)) |>
  select(1:2) |>
  unnest(cols = c(sumoftopics)) |>
  column_to_rownames("com_tp") |>
  as.matrix()
df_at_sum

# normalized by community size (community topic concentration)
df_at_dcsize <- df_at |>
  group_by(com_tp) |>
  summarise(sumoftopics=across(at_rn:at_ev, sum),
            numberofdocs=n()) |>
  # divide sum of topic percentage by number of docs within the community
  mutate(sumoftopics=sumoftopics/numberofdocs) |>
  select(1:2) |>
  unnest(cols=c(sumoftopics)) |>
  column_to_rownames("com_tp") |>
  as.matrix()
df_at_dcsize

## Choosing K ####
# k-means
set.seed(123)
k_g_at_result <- list()
k_g_at_ss <- c()
## loop from 1 cluster to existing number of network communities
for(i in 1:(nrow(df_at_dcsize)-1)){ # using normalized/concentration version
  k_g_at_result[[i]] <- kmeans(df_at_dcsize, centers = i)
  k_g_at_ss[i] <- k_g_at_result[[i]]$tot.withinss
}
plot(k_g_at_ss, type="b", ylab="Total within SS", xlab="k", main="K-means")

# if we choose k=2, network communities will be grouped as follows
k_g_at_result[[2]]$cluster

g_at_k <- g_at # copy graph
V(g_at_k)$k <- k_g_at_result[[2]]$cluster[V(g_at_k)$com_tp] # picking cluster id for k=2

as_data_frame(g_at_k, what="vertices") |>
  select(com_tp, k) |>
  print()

## Final result ####
## a plot with community and attributes
ggraph(g_at_k)+
  geom_edge_fan(arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(4, 'mm')) + 
  geom_node_point(aes(color = rgbsum, shape=as.factor(k)), size=8)+
  geom_node_text(aes(label=name), color="black", fontface="bold")+
  scale_color_identity()+
  scale_shape_manual(values=c("square", "asterisk", "circle"))+
  theme_void()+
  theme(plot.margin = margin(1, 1, 1, 1, "cm"), 
        legend.position = c(0.1,0.8))+
  guides(
    shape = guide_legend("Community")
  )+
  geom_point(aes(x=0.1,y=1.28,color="red"),size=4)+
  geom_point(aes(x=0.1,y=1.12,color="green"),size=4)+
  geom_point(aes(x=0.1,y=0.95,color="blue"),size=4)+
  geom_text(aes(x=0.01,y=1.2),
            label="Topic\n    CE\n    CS\n    EV",
            hjust=0,
            fontface="plain",
            lineheight=1.5) 



# 2 UN GENERAL ASSEMBLY ####

# data
load("ga_network.RData")
load("ga_stm.RData")

## Network ####
gorder(g1)
sum(degree(g1, mode="all")==0) 

## STM ####
summary(df_model)
head(topwords)


## Preparation for the fusion ####

## prepare network for the fusion
g1_pre <- g1_nodes_df %>%
  # only retain graph nodes that are also present at the stm
  filter(res_id2 %in% df_model$res_id2) %>%
  select(res_id2, cluster_wt) %>%
  arrange(by=res_id2)

## add meaningful labels based on most central resolution of the cluster
g1_lab <- igraph::as_data_frame(g1, what="vertices") %>%
  select(name, cluster_wt, in_deg) %>%
  group_by(cluster_wt) %>%
  mutate(n=n()) %>%
  slice_max(order_by = in_deg, n=1, with_ties = F)

## apply label to each community based on most cited res
g1_pre <- g1_pre %>%
  left_join(g1_lab, by="cluster_wt")

colnames(g1_pre) <- c("res_id2", "cluster_wt", "cluster_lab", "in_deg_top_res", "cl_order")


## prepare text topics for merge
stm_pre <- df_model %>%
  select(res_id2, Topic1:Topic40) %>%
  arrange(by=res_id2)

## topic labels
stm_lab <- topwords[1:3,] %>%
  pivot_longer(
    cols = 1:40,
    names_to = "Topic",
    values_to = "Words"
  ) %>%
  group_by(Topic) %>%
  summarise(topic_lab=str_c(Words, collapse = "_")) %>%
  mutate(ord=as.integer(str_extract(Topic,"[0-9]+"))) %>%
  arrange(ord)

## apply labels
colnames(stm_pre)[2:41] <- stm_lab$topic_lab


## Cluster-topic matrix ####
g1stm <- cbind(g1_pre[,c(1,3)], stm_pre[,2:41])

g1stm <- g1stm %>%
  column_to_rownames("res_id2") %>%
  group_by(cluster_lab) %>%
  # calculate volume and concentration
  summarise(sumoftopics=across(
    session_item_decemb:tortur_punish_prison,
    sum),
    numberofdocs=n()) %>%
  mutate(sumoftopics=sumoftopics/numberofdocs) %>% 
  select(1:2) %>%
  unnest(cols=c(sumoftopics)) %>%
  column_to_rownames("cluster_lab") %>%
  as.matrix()

## Heatmap
heatmap(t(g1stm),
        scale = "none",
        revC=T, 
        xlab = "Citation Clusters",
        ylab = NA)


## Choosing K ####

set.seed(123)
## range from 1000 to 25, by 25 decrements
rg <- seq(1000,25,by=-25)

## to store kmeans and modularity results
g1_pre_g <- induced_subgraph(g1, 
                             V(g1)$name %in% g1_pre$res_id2)
kresult <- list()
mresult <- list()
kss_mod <- data.frame(k=numeric(),
                      twss=numeric(),
                      mod=numeric())
for(i in seq_along(rg)){
  
  # store results for kss
  kresult[[i]] <- kmeans(g1stm, centers = rg[i])
  kss_mod[i,1] <- rg[i]
  kss_mod[i,2] <- kresult[[i]]$tot.withinss
  
  # calculate new modularities
  mresult[[i]] <- data.frame(cluster_lab=names(kresult[[i]]$cluster),
                             cluster_kmeans_id=kresult[[i]]$cluster) %>%
    full_join(g1_pre, by="cluster_lab") %>%
    arrange(match(res_id2, V(g1)$name))
  kss_mod[i,3] <- modularity(g1_pre_g,
                             mresult[[i]]$cluster_kmeans_id)
}

### plot modularity vs wss
par(mar=c(5,5,4,6))
plot(kss_mod$k, 
     kss_mod$twss, 
     type="b", 
     col="black",  
     ylab="Total WSS",
     xlab="K",
     axes=FALSE)
axis(2,
     ylim=c(0,250),
     col="black",
     las=1)
axis(1,
     ylim=c(0,1000),
     col="black")
box()
par(new=T)
plot(kss_mod$k, 
     kss_mod$mod, 
     type="l", 
     col="blue",
     ylab="",
     xlab="",
     axes=FALSE)
axis(4, 
     ylim=c(0.7,0.9),
     col="blue", 
     col.axis="blue", 
     las=1)


# retain k=125
## re-assign nodes
which(rg==125) # 36
length(unique(mresult[[36]]$cluster_kmeans_id)) # 125

g1_k125 <- mresult[[36]]

## Compare reduction in isolates ####

## for each res, size of old community vs new cluster
cl_order_k125 <- g1_k125 %>%
  group_by(cluster_kmeans_id) %>%
  tally()

g1_k125 <- g1_k125 %>%
  left_join(cl_order_k125, by="cluster_kmeans_id")
colnames(g1_k125) <- c("cluster_wt_lab", "cluster_kmeans_id","res_id2", "cluster_wt_id", "in_deg_top_res_clwt", "cluster_wt_order", "cluster_kmeans_order")

# compare mean size
length(unique(g1_lab$cluster_wt))
mean(g1_lab$n) # 6 res per cluster
length(unique(cl_order_k125$cluster_kmeans_id))
mean(cl_order_k125$n) # 143 res per cluster

# compare size of cluster to which each res belongs
ggplot(g1_k125, aes(x=cluster_wt_order, y=cluster_kmeans_order))+
  geom_point()+
  geom_abline()+
  labs(x="Size of network communities", y="Size of final clusters", title="Natural scale")

ggplot(g1_k125, aes(x=cluster_wt_order, y=cluster_kmeans_order))+
  geom_point()+ 
  geom_abline()+
  scale_y_log10(limits=c(1,3e3))+scale_x_log10(limits=c(1,3e3))+
  labs(x="Size of network communities", y="Size of final clusters", title="Log10 scale")

# Plot histogram
g1_k125_l <- g1_k125 %>%
  pivot_longer(cols=c(cluster_wt_order, cluster_kmeans_order),
               names_to = "cluster",
               values_to = "size") %>%
  mutate(bin = case_when(
    size == 1 ~ "1",
    size <= 10 ~ "2 - 10",
    size <= 100 ~ "11 - 100",
    size <= 1000 ~ "101 - 1000",
    TRUE ~ "1001 - 3000")) %>%
  mutate(bin=factor(bin, 
                    levels=c("1", "2 - 10","11 - 100","101 - 1000","1001 - 3000")))

ggplot(g1_k125_l, aes(x=bin, fill=cluster))+
  geom_histogram(stat = "count", position = "dodge")+
  scale_fill_manual(values=c("black","darkgrey"),
                    labels=c("Final clusters", "Network communities"))+
  labs(x="Community size", y="Frequency", fill="Clusters")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "top")
