######Sparse matrix#####

real_movies = as(movielens_data,"realRatingMatrix")
real_movies_mat=as(real_movies,"matrix")
real_movies_mat[is.na(real_movies_mat)]=0
copy_real_movies_mat = real_movies_mat
real_movies_den = density(real_movies_mat)




count_rate_per_user = apply(real_movies_mat,1, function(x) sum(x>0))

counts=rep(NA)

for (i in 1:943) {
  counts[i] = count_rate_per_user[[i]]
}

user_rate_mat = cbind(c(1:943),counts)

per = rep(NA)

for (i in 1:943) {
  per[i] = (user_rate_mat[i,2]/100000)*100
}

new_per = per/2

new_count = round(new_per*1000)

user_rate_mat_new = cbind(user_rate_mat,new_per,new_count)
user_rate_mat_new


##which(real_movies_mat[1,]>0)

spar_mat = rep(list(NA),943)

for (i in 1:943) {
  samp = c(which(real_movies_mat[i,]>0))
  spar_mat[[i]] = sample(samp,length(samp)/2)
}

for (i in 1:943) {
  real_movies_mat[i,spar_mat[[i]]]=0
}

sparse_mat = real_movies_mat
real_movies_mat = copy_real_movies_mat

id_mov=rep(list(NA),943)

for (i in 1:943) {
  id_mov[[i]]=which(sparse_mat[i,]>0)
}

times_rep=rep(list(NA),943)

for (i in 1:943) {
  times_rep[[i]] = length(id_mov[[i]])
}

rates = rep(list(NA),943)

w=0

for (i in 1:943) {
  for (j in 1:length(sparse_mat[i,id_mov[[i]]])) {
    w=c(w,sparse_mat[i,id_mov[[i]]][[j]]) 
  }
}

user_id=0
for (i in 1:943) {
  user_id =c(user_id,c(rep(i,length(id_mov[[i]])))) 
}

item_id=0
for ( i in 1:943) {
  item_id=c(item_id,id_mov[[i]])
}

sparse_mat_correct=cbind(user_id,item_id,w)
sparse_mat_correct=as.data.frame(sparse_mat_correct)