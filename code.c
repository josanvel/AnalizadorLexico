int main(){
	float numf = 1.2;
	int numI = 0;
	char cadena[10] = "jose";
	printf( "Ingrese un numero: " );
	scanf( "%s" ,&numI );
	if( numI >= 10){
		prinf( "Hola mundo!");
	}else{
		printf( "Mundo Hola");
	}
	for( int i = 0; i < numI; i++){
		printf( "*" );
	}
	while( numI == 0){
		printf( "-*-" );
		numI++;
	}
	/*jose*/
	do{
		printf( "*-*");
		numI++;
	}while( numI >= 0 );
	switch( numI ){
		case 1: printf( );
		break;
		default: printf( );
		break:
	}	
	return 0;
}
