#include "grid.h"

extern int gridRegisterIndex;

/* Some access methods for gpars */
double gpFontSize(SEXP gp) {
    return REAL(getListElement(gp, "fontsize"))[0];
}

double gpLineHeight(SEXP gp) {
    return REAL(getListElement(gp, "lineheight"))[0];
}

int gpCol(SEXP gp) {
    SEXP col = getListElement(gp, "col");
    int result;
    if (isNull(col))
	result = NA_INTEGER;
    else
	result = RGBpar(col, 0);
    return result;
}

SEXP gpFillSXP(SEXP gp) {
    return getListElement(gp, "fill");
}

int gpFill(SEXP gp) {
    SEXP fill = gpFillSXP(gp);
    int result;
    if (isNull(fill))
	result = NA_INTEGER;
    else
	result = RGBpar(fill, 0);
    return result;
}

double gpGamma(SEXP gp) {
    return REAL(getListElement(gp, "gamma"))[0];
}

int gpLineType(SEXP gp) {
    return LTYpar(getListElement(gp, "lty"), 0);
}

double gpLineWidth(SEXP gp) {
    return REAL(getListElement(gp, "lwd"))[0];
}

double gpCex(SEXP gp) {
    return REAL(getListElement(gp, "cex"))[0];
}

int gpFont(SEXP gp) {
    return INTEGER(getListElement(gp, "font"))[0];
}

SEXP L_setGPar(SEXP gpars) 
{
    /* Set the value of the current gpars on the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    setGridStateElement(dd, GSS_GPAR, gpars);
    return R_NilValue;
}

SEXP L_getGPar(SEXP gpars) 
{
    /* Get the value of the current gpars on the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_GPAR);
}

SEXP L_getGPsaved() 
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_GPSAVED);
}

SEXP L_setGPsaved(SEXP gpars) 
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    setGridStateElement(dd, GSS_GPSAVED, gpars);
    return R_NilValue;
}

void initGPar(GEDevDesc *dd)
{
    NewDevDesc *dev = dd->dev;
    SEXP gpar, gparnames;
    SEXP gpfill, gpcol, gpgamma, gplty, gplwd, gpcex, gpfs, gplh, gpfont;
    SEXP gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    PROTECT(gpar = allocVector(VECSXP, 9));
    PROTECT(gparnames = allocVector(STRSXP, 9));
    SET_STRING_ELT(gparnames, GP_FILL, mkChar("fill"));
    SET_STRING_ELT(gparnames, GP_COL, mkChar("col"));
    SET_STRING_ELT(gparnames, GP_GAMMA, mkChar("gamma"));
    SET_STRING_ELT(gparnames, GP_LTY, mkChar("lty"));
    SET_STRING_ELT(gparnames, GP_LWD, mkChar("lwd"));
    SET_STRING_ELT(gparnames, GP_CEX, mkChar("cex"));
    SET_STRING_ELT(gparnames, GP_FONTSIZE, mkChar("fontsize"));
    SET_STRING_ELT(gparnames, GP_LINEHEIGHT, mkChar("lineheight"));
    SET_STRING_ELT(gparnames, GP_FONT, mkChar("font"));
    setAttrib(gpar, R_NamesSymbol, gparnames);
    /* FIXME:  Need to export col2name via (probably) GraphicsEngine.h
     * In the meantime I just have to override the device settings
     */
    PROTECT(gpfill = allocVector(STRSXP, 1));
    /* SET_STRING_ELT(gpfill, 0, mkChar(col2name(dev->startfill))); */
    SET_STRING_ELT(gpfill, 0, mkChar("transparent"));
    SET_VECTOR_ELT(gpar, GP_FILL, gpfill);
    PROTECT(gpcol = allocVector(STRSXP, 1));
    /* SET_STRING_ELT(gpcol, 0, mkChar(col2name(dev->startcol))); */
    SET_STRING_ELT(gpcol, 0, mkChar("black"));
    SET_VECTOR_ELT(gpar, GP_COL, gpcol);
    PROTECT(gpgamma = allocVector(REALSXP, 1));
    REAL(gpgamma)[0] = dev->startgamma;
    SET_VECTOR_ELT(gpar, GP_GAMMA, gpgamma);
    PROTECT(gplty = LTYget(dev->startlty));
    SET_VECTOR_ELT(gpar, GP_LTY, gplty);
    PROTECT(gplwd = allocVector(REALSXP, 1));
    REAL(gplwd)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_LWD, gplwd);
    PROTECT(gpcex = allocVector(REALSXP, 1));
    REAL(gpcex)[0] = 1;
    SET_VECTOR_ELT(gpar, GP_CEX, gpcex);
    PROTECT(gpfs = allocVector(REALSXP, 1));
    REAL(gpfs)[0] = dev->startps;
    SET_VECTOR_ELT(gpar, GP_FONTSIZE, gpfs);
    PROTECT(gplh = allocVector(REALSXP, 1));
    REAL(gplh)[0] = 1.2;
    SET_VECTOR_ELT(gpar, GP_LINEHEIGHT, gplh);
    PROTECT(gpfont = allocVector(INTSXP, 1));
    INTEGER(gpfont)[0] = dev->startfont;
    SET_VECTOR_ELT(gpar, GP_FONT, gpfont);
    SET_VECTOR_ELT(gsd, GSS_GPAR, gpar);
    UNPROTECT(11);
}
