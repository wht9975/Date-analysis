library(tidyverse)
library(MetBrewer)
library(cowplot)
library(billboard)
library(extrafont)
library(cowplot)
library(showtext); showtext_auto()
library(ggcorrplot)
library(ggthemes)
library(ggrepel)
library(ggforce)
library(ggsci)
library(gghalves)
library(ggprism)
library(ggsignif)
library(car)
library(ggbump)
font_add_google("Satisfy", "satisfy")
font_add_google("Sacramento", "mento")
font_add_google("Lobster", "lobster")
font_add_google("Luckiest Guy", "luck")
font_add_google("Calligraffitti", "call")
font_add_google("Kalam", "kalam")
font_add_google("ZCOOL KuaiLe", "cool")
song = wiki_hot_100s
summary(song)

singer <- song %>%
  mutate(year = as.numeric(year),
         no = as.numeric(no)) %>%
  filter(year %in% 2016 & no <= 100) %>%
  group_by(artist) %>%
  mutate(best = min(no),
         num = n(),
         no_all = sum(no)) %>%
  ungroup %>%
  mutate(mean_no = no_all/num) %>%
  arrange(desc(num), mean_no) %>%
  group_by(artist) %>%
  mutate(rank = 1:num) %>%
  filter(rank == 1) %>%
  select(-c(year,rank,no)) %>%
  ungroup() %>%
  mutate(rank = row_number())

top_10 <- singer[1:10,] %>%
  mutate( angle = 360 - rank*(360 / (max(rank)+1)),
          angle = ifelse(angle < -90, angle +180, angle),
          mean_no = round(mean_no))


image = 'loveuself.png'

top_10 %>%
  ggplot()+
  ggimage::geom_image(aes(x = 0, y = -1.5, image = image),size=0.19) +
  geom_col(aes(x=rank, y=num, fill = artist),show.legend = F, width = 0.95)+
  coord_polar(theta = 'x',direction = 1,start = -0.3)+
  ylim(c(-1.5,5))+
  xlim(c(0,11))+
  theme_void()+
  geom_text(aes(x = rank, y=as.numeric(num)+1,label = artist),
            size=18, family = 'satisfy', fontface = 'bold', vjust = -0.1) +
  geom_text(aes(x = rank, y=as.numeric(num)+1,label = paste(title,'-',best)),
            size=11, family = 'call', fontface = 'bold', vjust = 1.7, color = '#716e77', hjust = 0.55) +
  geom_text(aes(x = 0.25, y=2.8),label = 'The year of',
           size=28, family = 'lobster', fontface = 'bold', hjust = 1)+
  geom_text(data = data.frame(label=factor(c('2','0','1','6')),
                              y = c(12/4, 9/4, 6/4, 3/4)),
            aes(x = c(0.5,0.5,0.45,0.5) , y = y , label = label),vjust = 1,hjust = 1,
            size = 25,  family = 'kalam', fontface = 'bold') +
  geom_text(aes(x = rank, y=as.numeric(num)/2, label = rank, size =rank),
            color = 'white',family = 'kalam',show.legend = F) +
  scale_size_continuous(range = c(32,16)) +
  scale_fill_manual(values = met.brewer('Hokusai1',n=10)) ->song_2016
ggsave("song_2016.pdf", song_2016,width = 20,height = 20,units = "in")




top_10 %>%
  ggplot(aes(x = rank, y = 100 - mean_no, fill = artist))+
  geom_col(aes(y=100),color = 'lightgrey',fill = 'white', lty = 'dashed', size = 0.8)+
  geom_col(aes(color = artist),show.legend = F,size = 0.8)+
  scale_fill_manual(values = met.brewer('Hokusai1',n=10))+
  scale_color_manual(values = met.brewer('Hokusai1',n=10))+
  geom_hline(yintercept = c(50,75),color = '#716e77',lty = 'dashed',size = 0.8)+
  geom_text(aes(y = -4, label = artist),
            size=14, family = 'satisfy', fontface = 'bold')+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        # axis.line.x = element_line(size = 2),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = 'kalam',size = 64, hjust = 0.5),
        plot.margin = margin(8,2,1,2,unit = 'mm')) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks=c(0,50,75,100, 120), labels = c('',50,25,1, '')) -> song_col




ggdraw()+
  draw_plot(song_2016, x = 0.025,y = 0.175,width = 0.95,height = 0.95)+
  draw_plot(song_col, x = 0.125, y = 0.025,width = 0.75,height = 0.35)+
  draw_plot_label(c("M","e","a","n"), x = rep((0.07),4), y = seq(0.35,0.15,length.out=4),
                  size = 96, family = 'kalam',hjust = 0.5)+
  draw_plot_label(c("R","a","n","k"), x = rep((0.095),4), y = seq(0.32,0.15,length.out=4),
                  size = 84, family = 'kalam',hjust = 0.5) -> song_all

ggsave("2016.png", song_all, width = 16, height = 9, units = 'in',bg = 'white')


#corralate


feather <-  spotify_track_data %>%
  rename(artist=artist_name,
         title = track_name) %>%
  group_by(year) %>%
  mutate(rank = 1:n()) %>%
  ungroup

feather_all <- feather  %>%
  select(-c(year,artist,artist_id,track_href,track_name,track_id,key,type,uri,analysis_url,duration_ms,time_signature,explicit))


cor = round(cor(feather_all, method = 'pearson'),2)
p.mat <- cor_pmat(feather_all)
# scale_color_distiller = cmap in matplotlib
ggcorrplot(cor, method = 'circle',hc.order = TRUE,
                      outline.color = "white",type = "lower",
                      lab = F, digits = 2,lab_size = 4, lab_col = 'black',
                      colors = c('#128b8e','#FFF5C3',"#fb3c3c"),
                      ggtheme = theme_minimal())+
  guides(fill = guide_colorbar(direction = 'horizontal',
                               barwidth = unit(12,'cm'),
                               ticks = F)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 28, angle = 30,family = 'call', hjust = 0.6, vjust = 0.9),
        axis.text.y = element_text(size = 28, family = 'call'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(20,3,3,3,unit = 'mm'),
        legend.title = element_blank(),
        legend.text = element_text(size = 20,family = 'mento'),
        legend.position = ('top')) -> corplot

ggsave("feather_cor.png", corplot,width = 20, height = 20, units = 'cm',bg = 'white',dpi = 300)


summary(feather)
feather_year <- feather %>%
  select(year, title, artist, danceability, valence, energy, instrumentalness,rank, speechiness) %>%
  group_by(year) %>%
  summarise(mean_dance = mean(danceability),
         mean_valence = mean(valence),
         mean_energy = mean(energy),
         mean_speechiness = mean(speechiness)) %>%
  pivot_longer(c(mean_dance,mean_valence,mean_energy,mean_speechiness), names_to = 'index', values_to = 'value')
feather_year %>%
  ggplot(aes(x = year, y = value, group = index, color = index))+
  geom_point(aes(fill = index, shape = index),size =2, alpha = 0.8)+
  geom_smooth(aes(fill= index),
              span = 0.6, size = 0.8,
              method = "loess",
              level=0.95,
              formula = 'y~x', alpha = 0.5) +
  scale_fill_manual(values = c('#2E94B9','#F0B775','#3b9a9c','#D25565'),labels = c('Danceability','Energy','Speechiness','Valence'))+
  scale_color_manual(values = c('#2E94B9','#F0B775','#3b9a9c','#D25565'),labels = c('Danceability','Energy','Speechiness','Valence'))+
  scale_shape_manual(values = c(21,22,23,24),labels = c('Danceability','Energy','Speechiness','Valence'))+
  theme_minimal()+
  labs(x = '', y = "Score", title = 'Feathers of each Year')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 18, family = 'call'),
        legend.position = 'top',
        axis.text.x = element_text(size = 18, angle = 15,family = 'call', hjust = 0.5, vjust = 0.9),
        axis.text.y = element_text(size = 24, family = 'call'),
        axis.title = element_text(size = 40, family = 'kalam'),
        plot.title = element_text(size = 60, family = 'kalam', hjust = 0.5)) ->feather_plot
ggsave("feather.png",feather_plot, width = 12,height = 6, units = 'in', bg = 'white')

compare <- feather %>%
  filter(year %in% c('1960','2015')) %>%
  select(year, danceability, valence, energy) %>%
  mutate(year = factor(year)) %>%
  group_by(year) %>%
  mutate(no = 1:n()) %>%
  ungroup %>%
  pivot_longer(c(danceability, valence, energy), names_to = 'index', values_to = 'value') %>%
  mutate(dotx = case_when(year == '1960' ~ 1.15,
                           year == '2015' ~ 1.85),
         vlinx = case_when(year == '1960' ~ 0.85,
                          year == '2015' ~ 2.15),
         fill = case_when(year == '1960' & index == 'danceability' ~ '#35999D',
                          year == '1960' & index == 'energy' ~ '#D0938B',
                          year == '1960' & index == 'valence' ~ '#D2B577',
                          year == '2015' & index == 'danceability' ~ '#356CA2',
                          year == '2015' & index == 'energy' ~ '#BA5555',
                          year == '2015' & index == 'valence' ~ '#BB683F'),
         value = as.numeric(value))
summary(compare)
#dance
data_dance <- compare[compare$fill == '#35999D'| compare$fill == '#356CA2',]
leveneTest(data_dance$value,factor(data_dance$fill),center=mean)
#energy
data_energy <- compare[compare$fill == '#D0938B'| compare$fill == '#BA5555',]
leveneTest(data_energy$value,factor(data_energy$fill),center=mean)
#valence
data_valence <- compare[compare$fill == '#D2B577'| compare$fill == '#BB683F',]
leveneTest(data_valence$value,factor(data_valence$fill),center=mean)
t.test(value ~ fill,data_dance, paired = F, var.equal = F)
t.test(value ~ fill,data_energy, paired = F, var.equal = T)
t.test(value ~ fill,data_valence, paired = F, var.equal = F)

ggplot(compare, aes(y=value))+
  geom_line(aes(x = dotx, group = no,fill = fill),position = position_dodge(0.2),alpha = 0.35, color = 'grey')+
  geom_point(aes(x = dotx, group = no,fill = fill),color = 'white',position = position_dodge(0.2), shape = 21, size =2,alpha = 0.6)+
  geom_half_violin(data = compare[compare$year == '1960',], aes(x = vlinx,fill = fill),side = 'l',size = 0.3, trim = T,alpha = 0.8,color = 'white')+
  geom_half_violin(data = compare[compare$year == '2015',], aes(x = vlinx,fill = fill),side = 'r',size = 0.3, trim = T,alpha = 0.8,color = 'white')+
  geom_boxplot(aes(x = vlinx, group = year), color = 'white', fill = '#B9C8E3', width = 0.2, size = 0.2, outlier.shape = NA,alpha = 0.5)+
  stat_summary(aes(x = vlinx, color = fill), fun.data = "mean_cl_normal", fun.args = list(conf.int = .95), geom = 'errorbar',
               width = 0.08,size = 0.5)+
  geom_signif(data = data.frame(index = c("danceability","energy",'valence')),
              aes(annotations=rep("***", 3),y_position=rep(1.05, 3), xmin=rep(0.8, 3), xmax=rep(2.2, 3)),
              tip_length=0.02, manual = T,textsize = 8,vjust = 0.5) +
  facet_grid(~index)+
  scale_fill_identity()+
  scale_color_identity()+
  theme_prism(base_line_size =0.5)+
  scale_x_continuous(expand = c(0,0),breaks = c(1,2),labels = c("1960", "2015"),limits = c(0,3))+
  labs(x = 'Year', y= 'Score', title = '1960 v.s. 2015')+
  theme(plot.margin = margin(2,2,2,2,unit = 'mm'),
        panel.grid.minor = element_line(size = 0.4,color = "#e5e5e5"),
        panel.grid.major = element_line(size = 0.4,color = "#e5e5e5"),
        panel.spacing = unit(0,"lines"),
        axis.ticks = element_blank(),
        plot.title = element_text(family = 'kalam', size =48),
        axis.title = element_text(family = 'kalam', size =24),
        axis.text = element_text(family = 'mento', size =24),
        strip.text = element_text(family = 'kalam', size =32,vjust = 1)) ->compare_plot
ggsave('wind&rain.png',compare_plot,width=12,height = 6)



