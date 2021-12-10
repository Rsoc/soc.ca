object


# Calculate averages for a matrix -----



coord <- supplementary.categories(result, active, dim = 3:4)
coord <- extract_mod(result)
# Make minimal plots ----

p <- coord %>% map.ca.base(data = ., mapping = aes(x = X, y = Y, label = label), up = "UP", down = "DOWN", left = "LEFT", right = "RIGHT", base_size = 10)
p + geom_text() + facet_wrap(~Variable, ncol = 2) 
