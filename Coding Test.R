a = c(3, 6, 9, 14)
b = c(2, 4, 5, 8, 21)


c = vector(length = length(a) + length(b))


for (i in 1:4)
{
  if (a[i] < b[i]){
    c[2*i-1] = a[i];
    c[2*i] = b[i];
    
    
    
  }
  else {
    c[2*i-1] = b[i];
    c[2*i] = a[i]
  }
}

m = 1
for (i in 1:4)
{
  if (a[i] < b[m]){
    c[2*i-1]= a[i];
    c[2*i]= b[m];
    m = i
  }
  else {
    c[2*i-1]=b[m];
    c[2*i]= a[i];
    m= i+1
  }
}


if (i<m){
  c[2*i-1]=
}

if (i > m)
