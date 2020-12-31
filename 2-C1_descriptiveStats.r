# [2-C1_descriptiveStats.r]

## ---------------- create pauser -----------------------------------------------------------
pause <- function(x=2.5)
{                                                                                                                                                                                                                                                                                                            
	message("\n || >>> current file: [2-C1_descriptiveStats.r]")
	message("\n\t Preparing [next] code chunck...")
	p1 <- proc.time()
        Sys.sleep(x)
        proc.time() - p1                                                                                                                                                                                                                                                                 
}

## ---------------- descriptive stats -------------------------------------------------------
message("\n\n | [2.] exploring data...")
message(" --------------------------------------------------------------------")
# view summary statistics
message("\t\n\n\n || [2.0.1] viewing summary statistics...")
View(summary(trn))
View(summary(tst.pre))
View(summary(tst.final))
pause()
# total_accel_belt vs user_name
message("\t\n\n\n || [2.0.1] plotting [total_accel_belt vs user_name] (sorted)...")
srch <- df.trn$classe == "A"
df <- df.trn[srch,] %>% select(user_name,total_accel_belt)
df <- df %>% group_by(user_name) %>% summarize(total_accel_belt=sum(total_accel_belt)) %>%
       arrange(desc(total_accel_belt))
df <- df %>% mutate(user_name=fct_reorder(user_name,total_accel_belt))
p1 <- ggplot(data=df,mapping=aes(x=user_name,y=total_accel_belt,fill=(user_name == "adelmo")))
p1 <- p1 + geom_bar(stat="identity",width=0.2) + coord_flip()
p1 <- p1 + scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="grey45"))
p1 <- p1 + labs(title = "total_accel_belt vs. user_name",
		x = "user_name",
		y = "total_accel_belt")
p1 <- p1 + my_theme() + theme(legend.position = "none")
p1 %>% print()
pause()
# total_accel_arm vs user_name
message("\t\n\n\n || [2.0.2] plotting [total_accel_arm vs user_name] (sorted)...")
srch <- df.trn$classe == "A"
df <- df.trn[srch,] %>% select(user_name,total_accel_arm)
df <- df %>% group_by(user_name) %>% summarize(total_accel_arm=sum(total_accel_arm)) %>%
       arrange(desc(total_accel_arm))
df <- df %>% mutate(user_name=fct_reorder(user_name,total_accel_arm))
p2 <- ggplot(data=df,mapping=aes(x=user_name,y=total_accel_arm,fill=(user_name == "jeremy")))
p2 <- p2 + geom_bar(stat="identity",width=0.2) + coord_flip()
p2 <- p2 + scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="grey45"))
p2 <- p2 + labs(title = "total_accel_arm vs. user_name",
		x = "user_name",
		y = "total_accel_arm")
p2 <- p2 + my_theme() + theme(legend.position = "none")
p2 %>% print()
pause()
# total_accel_dumbbell vs user_name
message("\t\n\n\n || [2.0.3] plotting [total_accel_dumbbell vs user_name] (sorted)...")
srch <- df.trn$classe == "A"
df <- df.trn[srch,] %>% select(user_name,total_accel_dumbbell)
df <- df %>% group_by(user_name) %>% summarize(total_accel_dumbbell=sum(total_accel_dumbbell)) %>%
       arrange(desc(total_accel_dumbbell))
df <- df %>% mutate(user_name=fct_reorder(user_name,total_accel_dumbbell))
p3 <- ggplot(data=df,mapping=aes(x=user_name,y=total_accel_dumbbell,fill=(user_name == "jeremy")))
p3 <- p3 + geom_bar(stat="identity",width=0.2) + coord_flip()
p3 <- p3 + scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="grey45"))
p3 <- p3 + labs(title = "total_accel_dumbbell vs. user_name",
		x = "user_name",
		y = "total_accel_dumbbell")
p3 <- p3 + my_theme() + theme(legend.position = "none")
p3 %>% print()
pause()
# total_accel_forearm vs user_name
message("\t\n\n\n || [2.0.4] plotting [total_accel_forearm vs user_name] (sorted)...")
srch <- df.trn$classe == "A"
df <- df.trn[srch,] %>% select(user_name,total_accel_forearm)
df <- df %>% group_by(user_name) %>% summarize(total_accel_forearm=sum(total_accel_forearm)) %>%
       arrange(desc(total_accel_forearm))
df <- df %>% mutate(user_name=fct_reorder(user_name,total_accel_forearm))
p4 <- ggplot(data=df,mapping=aes(x=user_name,y=total_accel_forearm,fill=(user_name == "adelmo")))
p4 <- p4 + geom_bar(stat="identity",width=0.2) + coord_flip()
p4 <- p4 + scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="grey45"))
p4 <- p4 + labs(title = "total_accel_forearm vs. user_name",
		x = "user_name",
		y = "total_accel_forearm")
p4 <- p4 + my_theme() + theme(legend.position = "none")
p4 %>% print()
pause()
# view all plots
message("\t\n\n\n || [2.0.5] viewing all plots...")
plot <- ggarrange(p1,p2,p3,p4, nrow=2, ncol=2)
plot %>% print()
ggsave("./report_and_figures/figures/plot1.png",plot,dpi=500,width=45,height=25,units="cm",device="png")