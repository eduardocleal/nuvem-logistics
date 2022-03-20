# inspect data
df <- read.csv('logistics-case-v3.txt', as.is = TRUE, encoding = "UTF-8")
dim(df)
head(df)
summary(df)

# backup data
df_backup <- df

# update labels
df_labels <- c('id', 'created', 'transit_to_local', 'at_local',
               'transit_to_delivery', 'delivery', 'delivery_estimate',
               'destination_city', 'destination_uf')
names(df) <- df_labels

datetime_vars <- c('created', 'transit_to_local', 'at_local',
                   'transit_to_delivery', 'delivery')

# change data types
df$delivery_estimate <- as.Date(df$delivery_estimate)
for (variable in datetime_vars) {
  df[,variable] <- as.POSIXct(df[,variable], format='%Y-%m-%dT%H:%M:%S',
                            tz='America/Sao_Paulo')
}

# delay rate
df$delay <- as.Date(df$delivery) > df$delivery_estimate

# calculate delivery times
df$shipping_time <- difftime(df$transit_to_local, df$created, units = 'days')
df$transit_time <- difftime(df$transit_to_delivery, df$transit_to_local,
                            units = 'days')
df$delivery_time <- difftime(df$delivery, df$transit_to_delivery,
                             units = 'days')
df$total_time <- difftime(df$delivery, df$created, units = 'days')
df$delay_time <- (df$delay) * difftime(df$delivery, df$delivery_estimate,
                                       units = 'days')

turnaround <- c('shipping_time', 'transit_time', 'delivery_time', 'total_time')

# for (var in turnaround){
#   plot(density(na.omit(as.double(df[,var]))), xlab = 'Time (days)',
#        main = paste0(var, ', max = ',
#                      round(max(df[,var], na.rm = TRUE), 2), ' days'))
# }

# aggregate by destination_uf
mean(df$delay, na.rm = TRUE)
delay_by_uf <- aggregate(cbind(total_time, delay) ~ destination_uf,
                         FUN = mean, data = df)
delay_by_uf[order(delay_by_uf$delay, decreasing = TRUE),]

times_by_uf <- aggregate(cbind(shipping_time, transit_time, delivery_time,
                               total_time, delay_time) ~ destination_uf,
                         FUN = mean, data = df[df$delay,])
times_by_uf[order(times_by_uf$delay_time, decreasing = TRUE),]


df_save <- na.omit(df[,df_labels != 'destination_city'])

lapply(list(df, df_save), dim)

# save down file
write.csv(df_save, 'df.csv', row.names = FALSE)

# take another look at data
summary(df)