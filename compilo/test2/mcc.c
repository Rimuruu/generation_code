


long mcc(long n){
    if(n>100) {
        return (n-10);
    }
    return mcc(mcc(n+11));
}

int main(){
    return mcc(122);
}