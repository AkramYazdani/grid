#include "grid.h"

int gridRegisterIndex;

SEXP createGridSystemState()
{
    return allocVector(VECSXP, 9);
}

void initDL(GEDevDesc *dd)
{
    SEXP dl, dlindex;
    SEXP vp = gridStateElement(dd, GSS_VP);
    SEXP gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    /* The top-level viewport goes at the start of the display list
     */
    PROTECT(dl = allocVector(VECSXP, 100));
    SET_VECTOR_ELT(dl, 0, vp);
    SET_VECTOR_ELT(gsd, GSS_DL, dl);
    PROTECT(dlindex = allocVector(INTSXP, 1));
    INTEGER(dlindex)[0] = 1;
    SET_VECTOR_ELT(gsd, GSS_DLINDEX, dlindex);
    UNPROTECT(2);
}

void fillGridSystemState(SEXP state, GEDevDesc* dd) 
{
    SEXP devsize, currloc, dlon;
    PROTECT(devsize = allocVector(REALSXP, 2));
    REAL(devsize)[0] = 0;
    REAL(devsize)[1] = 0;
    SET_VECTOR_ELT(state, GSS_DEVSIZE, devsize);
    /* "current location"
     * Initial setting relies on the fact that all values sent to devices
     * are in INCHES;  so (0, 0) is the bottom-left corner of the device.
     */
    PROTECT(currloc = allocVector(REALSXP, 2));
    REAL(currloc)[0] = 0;
    REAL(currloc)[1] = 0;    
    SET_VECTOR_ELT(state, GSS_CURRLOC, currloc);
    PROTECT(dlon = allocVector(LGLSXP, 1));
    LOGICAL(dlon)[0] = TRUE;
    SET_VECTOR_ELT(state, GSS_DLON, dlon);
    initGPar(dd);
    SET_VECTOR_ELT(state, GSS_GPSAVED, R_NilValue);
    /* Create a top-level viewport for this device
     */
    initVP(dd);
    /* The top-level viewport goes at the start of the display list
     */
    initDL(dd);
    SET_VECTOR_ELT(state, GSS_GLOBALINDEX, R_NilValue);
    UNPROTECT(3);
}

SEXP gridStateElement(GEDevDesc *dd, int elementIndex)
{
    return VECTOR_ELT((SEXP) dd->gesd[gridRegisterIndex]->systemSpecific, 
		      elementIndex);
}

void setGridStateElement(GEDevDesc *dd, int elementIndex, SEXP value)
{
    SET_VECTOR_ELT((SEXP) dd->gesd[gridRegisterIndex]->systemSpecific, 
		   elementIndex, value);
}

static void deglobaliseState(SEXP state)
{
    int index = INTEGER(VECTOR_ELT(state, GSS_GLOBALINDEX))[0];
    SET_VECTOR_ELT(getSymbolValue(".GRID.STATE"), index, R_NilValue);
}

static int findStateSlot()
{
    int i;
    int result = -1;
    SEXP globalstate = getSymbolValue(".GRID.STATE");
    for (i = 0; i < length(globalstate); i++)
	if (VECTOR_ELT(globalstate, i) == R_NilValue) {
	    result = i;
	    break;
	}
    if (result < 0)
	error("Unable to store grid state.  Too many devices open?");
    return result;
}

static void globaliseState(SEXP state)
{
    int index = findStateSlot();
    SEXP globalstate, indexsxp;
    PROTECT(globalstate = getSymbolValue(".GRID.STATE"));
    /* Record the index for deglobalisation
     */
    PROTECT(indexsxp = allocVector(INTSXP, 1));
    INTEGER(indexsxp)[0] = index;
    SET_VECTOR_ELT(state, GSS_GLOBALINDEX, indexsxp);
    SET_VECTOR_ELT(globalstate, index, state);
    UNPROTECT(2);
}

SEXP gridCallback(GEevent task, GEDevDesc *dd, SEXP data) {
    SEXP result = R_NilValue;
    SEXP valid;
    SEXP gridState;
    GESystemDesc *sd;
    SEXP currentgp;
    SEXP fill;
    SEXP gsd;
    SEXP devsize;
    switch (task) {
    case GE_InitState:
	/* FIXME: Gross hack to stop the base graphics moaning at me.
	 * This should be removed when base graphics have been 
	 * properly split from the graphics engine.
	 */
	/* This is dangerous if base graphics are used in same device
	 * because could then do base graphics operations that 
	 * base graphics would normally not allow because 
	 * plot.new() has not been called
	 */
	/* There is further danger that base graphics could switch it
	 * off again, thereby incapacitating grid graphics.
	 * For example, this might happen if a device is made too
	 * small.
	 */
	GSetState(1, (DevDesc*) dd);
	/* Even grosser hack to stop base graphics moaning
	 */
	/* This is currently the only way to set gpptr(dd)->valid
	 * Needs fixing (in base graphics)!
	 */
	/* NOTE that this needs to go before the createGridSystemState
	 * so that the silly clipping region it sets will get
	 * overridden by the top-level viewport
	 */
	GNewPlot(FALSE);
	/* Create the initial grid state for a device
	 */
	PROTECT(gridState = createGridSystemState());
	/* Store that state with the device for easy retrieval
	 */
	sd = dd->gesd[gridRegisterIndex];
	sd->systemSpecific = (void*) gridState;
	/* Initialise the grid state for a device
	 */
	fillGridSystemState(gridState, dd);
	/* Also store the state beneath a top-level variable so
	 * that it does not get garbage-collected
	 */
	globaliseState(gridState);
	UNPROTECT(1);
	break;
    case GE_FinaliseState:
	sd = dd->gesd[gridRegisterIndex];
	/* Simply detach the system state from the global variable
	 * and it will be garbage-collected
	 */
	deglobaliseState((SEXP) sd->systemSpecific);
	/* Also set the device pointer to NULL
	 */
	sd->systemSpecific = NULL;	
	break;
    case GE_SaveState:
	break;
    case GE_RestoreState:
	/* The graphics engine is about to replay the display list
	 * So we "clear" the device and reset the grid graphics state
	 */
	gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
	PROTECT(devsize = allocVector(REALSXP, 2));
	getDeviceSize(dd, &(REAL(devsize)[0]), &(REAL(devsize)[1]));
	SET_VECTOR_ELT(gsd, GSS_DEVSIZE, devsize);
	UNPROTECT(1);
	currentgp = gridStateElement(dd, GSS_GPAR);
	fill = gpFillSXP(currentgp);
	gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
	if (isNull(fill))
	    GENewPage(NA_INTEGER, gpGamma(currentgp), dd);
	else
	    GENewPage(RGBpar(fill, 0), gpGamma(currentgp), dd);
	initGPar(dd);
	initVP(dd);
	break;
    case GE_CopyState:
	break;
    case GE_CheckPlot:
	PROTECT(valid = allocVector(LGLSXP, 1));
	LOGICAL(valid)[0] = TRUE;
	UNPROTECT(1);
	result = valid;
    case GE_SaveSnapshotState:
	break;
    case GE_RestoreSnapshotState:
	break;
    case GE_ScalePS:
	break;
    }
    return result;
}

