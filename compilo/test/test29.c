long f(long n){
  if(n<=1){
	return 1;
}else{
	return 1+ (f(n-2)*2);
}
}

int main(){
	return f(5);
}
