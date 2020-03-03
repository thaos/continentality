#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void F77_NAME(compute_continentality_f)(
    int NX,
    int NY,
    int NBmois,
    double X1,
    double X2,
    double Y1,
    double Y2,
    double dlon,
    double dlat,
    double *lon,
    double *lat,
    double *Udsc,
    double *Vdsc,
    int *mask,
    double *Aco,
    double *Dco
);

extern SEXP c_compute_continentality_f(
    SEXP NX,
    SEXP NY,
    SEXP NBmois,
    SEXP X1,
    SEXP X2,
    SEXP Y1,
    SEXP Y2,
    SEXP dlon,
    SEXP dlat,
    SEXP lon,
    SEXP lat,
    SEXP Udsc,
    SEXP Vdsc,
    SEXP mask
 ){
  SEXP Aco;
  SEXP Dco;
  printf("inside C");
  PROTECT(Aco = allocVector(REALSXP, asInteger(NX) * asInteger(NY) * asInteger(NBmois)));
  PROTECT(Dco = allocVector(REALSXP, asInteger(NX) * asInteger(NY)));
  SEXP out = PROTECT(allocVector(VECSXP, 2));
  printf("NX %d NY %d NBmois %d", asInteger(NX), asInteger(NY), asInteger(NBmois));
  printf("X1 %d X2 %d Y1 %d Y2 %d", asInteger(X1), asInteger(X2), asInteger(Y1), asInteger(Y2));
  F77_CALL(compute_continentality_f)(
    asInteger(NX),
    asInteger(NY),
    asInteger(NBmois),
    asReal(X1),
    asReal(X2),
    asReal(Y1),
    asReal(Y2),
    asReal(dlon),
    asReal(dlat),
    REAL(lon),
    REAL(lat),
    REAL(Udsc),
    REAL(Vdsc),
    INTEGER(mask),
    REAL(Aco),
    REAL(Dco)
  );
  SET_VECTOR_ELT(out, 0, Aco);
  SET_VECTOR_ELT(out, 1, Dco);
  UNPROTECT(3);
  return(out);
}

// Routine registration
static const R_CallMethodDef CallEntries[] = {
  {"c_compute_continentality_f",   (DL_FUNC) &c_compute_continentality_f,   14},
  {NULL,         NULL,                0}
};
 
void R_init_continentality(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
