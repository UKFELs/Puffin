/* Simple code to write integer */
#include <stdio.h>
#include <string.h>

/* differit fortran compilers have differit translation rules
   for C functions name.
   We provide below for one underscore and two underscores.
*/

#ifdef ONE_UNDERSCORE
  #define OPENFILEFOROUTPUT openfileforoutput_c_
  #define WRITEINTEGER      writeinteger_c_
  #define WRITEDOUBLE       writedouble_c_
  #define WRITESTRING       writestring_c_
  #define WRITELONGARRAY    writelongarray_c_
  #define WRITEDOUBLEARRAY  writedoublearray_c_
#elif SECOND_UNDERSCORE
  #define OPENFILEFOROUTPUT openfileforoutput_c__
  #define WRITEINTEGER      writeinteger_c__
  #define WRITEDOUBLE       writedouble_c__
  #define WRITESTRING       writestring_c__
  #define WRITELONGARRAY    writelongarray_c__
  #define WRITEDOUBLEARRAY  writedoublearray_c__
#else
  #define OPENFILEFOROUTPUT openfileforoutput_c
  #define WRITEINTEGER      writeinteger_c
  #define WRITEDOUBLE       writedouble_c
  #define WRITESTRING       writestring_c
  #define WRITELONGARRAY    writelongarray_c
  #define WRITEDOUBLEARRAY  writedoublearray_c
#endif

void OPENFILEFOROUTPUT (char *zFileName, int iFileNameLength)
{
   FILE *fp;
   int i;
   char zfile[(iFileNameLength)]; 
   for(i=0; i<(iFileNameLength); i++)
{
  zfile[i] = zFileName[i];
}
   fp = fopen(zfile,"wb");
   fclose(fp);
}

void WRITEINTEGER (char *zFileName,  int *iNum)
{
   FILE *fp;   
   fp = fopen(zFileName,"ab"); 
   fwrite(iNum, sizeof(int),1,fp);
   fclose(fp);
}

void WRITEDOUBLE (char *zFileName,  double *sReal)
{
   FILE *fp;   
   fp = fopen(zFileName,"ab"); 
   fwrite(sReal, sizeof(double),1,fp);
   fclose(fp);
}

void WRITESTRING (char *zFileName, char *zString, int iLength, int iStringLength)
{
   FILE *fp;  
   fp = fopen(zFileName,"ab"); 
   fwrite(zString, iStringLength,1,fp);
   fclose(fp);
}


void WRITELONGARRAY (char *zFileName, long *iNum, int *iArrayLength )
{
   FILE *fp; 
   fp = fopen(zFileName,"ab"); 
   fwrite(iNum, sizeof(long),*iArrayLength,fp);
   fclose(fp);
}

void WRITEDOUBLEARRAY (char *zFileName,  double *sReal, int *iArrayLength)
{
   FILE *fp;   
   fp = fopen(zFileName,"ab"); 
   fwrite(sReal, sizeof(double),*iArrayLength,fp);
   fclose(fp);
}
