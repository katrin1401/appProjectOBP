crptw_instances = readLines("D:/vu-master/Project OBP/Container_Data/Data/crptw_instance/instances.txt")
arrival_instances = readLines("D:/vu-master/Project OBP/Container_Data/Data/Arrival instance/instances.txt")

bay = read_crptw(crptw_instances[10])

read_crptw = function(filepath){
  # read first line separately
  header = unname(read.table(filepath, nrows=1, sep="\n", stringsAsFactors = FALSE))[[1]][1]
  stacks = levels(unname(read.table(filepath, sep="\n", skip=1))[[1]])
  
  #extract dimensions of bay from first line
  dims = strsplit(strsplit(header, " ")[[1]][1], "_")[[1]][2]
  n_tiers = type.convert(substr(dims, 3, 4))
  n_stacks = type.convert(substr(dims, 1, 2))
  
  # create bay
  bay = matrix(nrow=n_tiers, ncol=n_stacks)
  
  # fill bay with containers
  for (i in 1:n_stacks){
    containers = as.numeric(strsplit(stacks[i], "\\D+")[[1]][-1])[-c(1,2,3)]
    if(length(containers)!=0){
      sequence = seq(1, length(containers), 2)
      for(j in 1:length(sequence)){
        bay[j,i] = containers[sequence[j]]
      }
    }
  }
  return(bay[nrow(bay):1,])
}

read_arr = function(filepath){
  arrivals = read.csv(filepath, sep=",")
  names(arrivals) = c("n", "arr", "dep")
  return(arrivals[order(arrivals$arr),])
}

