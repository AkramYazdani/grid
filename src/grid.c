#include "grid.h"

/* NOTE:
 * The extensive use of L or L_ prefixes dates back to when this 
 * package used to be called "lattice"
 */

extern int gridRegisterIndex;

/* FIXME:  Need to query device for this
 */
double devPS = 10;

void getDeviceSize(GEDevDesc *dd, double *devWidthCM, double *devHeightCM) 
{
    double left, right, bottom, top;
    dd->dev->size(&left, &right, &bottom, &top, dd->dev);
    *devWidthCM = fabs(right - left) * dd->dev->ipr[0] * 2.54;
    *devHeightCM = fabs(top - bottom) * dd->dev->ipr[1] * 2.54;
}

Rboolean deviceChanged(double devWidthCM, double devHeightCM, 
		       GEDevDesc* dd)
{
    Rboolean result = FALSE;
    SEXP devsize;
    PROTECT(devsize = gridStateElement(dd, GSS_DEVSIZE));
    if (fabs(REAL(devsize)[0] - devWidthCM) > DBL_EPSILON) {
	result = TRUE;
	REAL(devsize)[0] = devWidthCM;
    }
    if (fabs(REAL(devsize)[1] - devHeightCM) > DBL_EPSILON) {
	result = TRUE;
	REAL(devsize)[1] = devHeightCM;
    }
    UNPROTECT(1);
    return result;
}

/* Register grid with R's graphics engine
 */
SEXP L_initGrid() 
{
    SEXP globalstate;
    /* 64 comes from the maximum number of R devices allowed
     * to be open at one time 
     * See:  R_MaxDevices in Graphics.h
     */
    PROTECT(globalstate = allocVector(VECSXP, 64));
    setSymbolValue(".GRID.STATE", globalstate);
    GEregisterSystem(gridCallback, &gridRegisterIndex);
    UNPROTECT(1);
    return R_NilValue;
}

SEXP L_killGrid() 
{
    GEunregisterSystem(gridRegisterIndex);
    /* FIXME: Should "remove" the variable, not just NULL it
     */
    setSymbolValue(".GRID.STATE", R_NilValue);
    return R_NilValue;
}

/* Get the current device -- create one if there isn't one already
 */
GEDevDesc* getDevice() 
{
    if (NoDevices()) {
	SEXP defdev = GetOption(install("device"), R_NilValue);
	if (isString(defdev) && length(defdev) > 0) {
	    PROTECT(defdev = lang1(install(CHAR(STRING_ELT(defdev, 0)))));
	}
	else error("No active or default device");
	eval(defdev, R_GlobalEnv);
	UNPROTECT(1);
    }
    return GEcurrentDevice();
}

/* If this is the first time that a grid operation has occurred for 
 * this device, do some initialisation.
 * NOTE that this does some things that make base R graphics risky
 * on the device hereafter.
 */
void dirtyGridDevice(GEDevDesc *dd) {
    SEXP gsd, griddev;
    if (!LOGICAL(gridStateElement(dd, GSS_GRIDDEVICE))[0]) {
	/* Record the fact that this device has now received grid output
	 */
	gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
	PROTECT(griddev = allocVector(LGLSXP, 1));
	LOGICAL(griddev)[0] = TRUE;
	SET_VECTOR_ELT(gsd, GSS_GRIDDEVICE, griddev);
	UNPROTECT(1);
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
	/* Create a top-level viewport for this device
	 */
	initVP(dd);
	/* The top-level viewport goes at the start of the display list
	 */
	initDL(dd);
    }
}

SEXP L_gridDirty()
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    dirtyGridDevice(dd);
    /* Ensure that base graphics (the GCheckState call in do_dotgraphics)
     * will not complain.
     * FIXME:
     * This should come out once more changes to base graphics have been made.
     */
    GSetState(1, (DevDesc*) dd);
    return R_NilValue;
}

void getViewportContext(SEXP vp, LViewportContext *vpc)
{
    fillViewportContextFromViewport(vp, vpc);
}

SEXP L_currentViewport() 
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_VP);
}

SEXP doSetViewport(SEXP vp, SEXP hasParent, GEDevDesc *dd)
{
    int i, j;
    double devWidthCM, devHeightCM;
    double xx1, yy1, xx2, yy2;
    SEXP currentClip;
    /* Get the current device size 
     */
    getDeviceSize((dd), &devWidthCM, &devHeightCM);
    if (hasParent != R_NilValue)
	/* Set the viewport's parent
	 * Need to do this in here so that redrawing via R BASE display
	 * list works 
	 */
	setListElement(vp, "parent", gridStateElement(dd, GSS_VP));
    /* Calculate the transformation for the viewport.
     * This will hopefully only involve updating the transformation
     * from the previous viewport.
     * However, if the device has changed size, we will need to
     * recalculate the transformation from the top-level viewport
     * all the way down.
     * NEVER incremental for top-level viewport
     */
    calcViewportTransform(vp, viewportParent(vp), 
			  hasParent != R_NilValue &&
			  !deviceChanged(devWidthCM, devHeightCM, dd), dd);
    /* If we are supposed to clip to this viewport ...
     * NOTE that we will only clip if there is no rotation
     */
    if (viewportClip(vp)) {
	double rotationAngle = REAL(viewportCurrentRotation(vp))[0];
	if (rotationAngle != 0)
	    warning("Cannot clip to rotated viewport");
	else {
	    /* Calculate a clipping region and set it
	     */
	    SEXP x1, y1, x2, y2;
	    LViewportContext vpc;
	    double vpWidthCM = REAL(viewportCurrentWidthCM(vp))[0];
	    double vpHeightCM = REAL(viewportCurrentHeightCM(vp))[0];
	    LTransform transform;
	    for (i=0; i<3; i++)
		for (j=0; j<3; j++)
		    transform[i][j] = 
			REAL(viewportCurrentTransform(vp))[i + 3*j];
	    if (hasParent == R_NilValue) {
		/* Special case for top-level viewport.
		 * Set clipping region outside device boundaries.
		 * This means that we have set the clipping region to
		 * something, but avoids problems if the nominal device
		 * limits are actually within its physical limits
		 * (e.g., PostScript)
		 */
	        PROTECT(x1 = unit(-.5, L_NPC));
		PROTECT(y1 = unit(-.5, L_NPC));
		PROTECT(x2 = unit(1.5, L_NPC));
		PROTECT(y2 = unit(1.5, L_NPC));
	    } else {
		PROTECT(x1 = unit(0, L_NPC));
		PROTECT(y1 = unit(0, L_NPC));
		PROTECT(x2 = unit(1, L_NPC));
		PROTECT(y2 = unit(1, L_NPC));
	    }
	    getViewportContext(vp, &vpc);
	    transformLocn(x1, y1, 0, vpc,  
			  viewportFontFamily(vp), 
			  viewportFont(vp),
			  viewportFontSize(vp),
			  viewportLineHeight(vp),
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx1, &yy1);
	    transformLocn(x2, y2, 0, vpc,  
			  viewportFontFamily(vp), 
			  viewportFont(vp),
			  viewportFontSize(vp),
			  viewportLineHeight(vp),
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx2, &yy2);
	    UNPROTECT(4);  /* unprotect x1, y1, x2, y2 */
	    /* The graphics engine only takes device coordinates
	     */
	    xx1 = toDeviceX(xx1, GE_INCHES, dd);
	    yy1 = toDeviceY(yy1, GE_INCHES, dd);
	    xx2 = toDeviceX(xx2, GE_INCHES, dd);
	    yy2 = toDeviceY(yy2, GE_INCHES, dd);
	    GESetClip(xx1, yy1, xx2, yy2, dd);
	}
    } else {
	/* If we haven't set the clipping region for this viewport
	 * we need to save the clipping region from its parent
	 * so that when we pop this viewport we can restore that.
	 */
	/* NOTE that we are relying on grid.R setting clip=TRUE
	 * for the top-level viewport, else *BOOM*!
	 */
	SEXP parentClip;
	PROTECT(parentClip = viewportCurClip(viewportParent(vp)));
	xx1 = REAL(parentClip)[0];
	yy1 = REAL(parentClip)[1];
	xx2 = REAL(parentClip)[2];
	yy2 = REAL(parentClip)[3];
	UNPROTECT(1);
    }
    PROTECT(currentClip = allocVector(REALSXP, 4));
    REAL(currentClip)[0] = xx1;
    REAL(currentClip)[1] = yy1;
    REAL(currentClip)[2] = xx2;
    REAL(currentClip)[3] = yy2;
    setListElement(vp, "cur.clip", currentClip);
    UNPROTECT(1);
    return vp;
}

SEXP L_setviewport(SEXP vp, SEXP hasParent)
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    vp = doSetViewport(vp, hasParent, dd);
    /* Set the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    setGridStateElement(dd, GSS_VP, vp);
    return R_NilValue;
}

/* This is similar to L_setviewport, except that it will do 
 * NOTHING if the device has not changed size
 */
SEXP L_unsetviewport(SEXP last)
{
    double xx1, yy1, xx2, yy2;
    SEXP parentClip;
    SEXP newvp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    /* Get the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */    
    SEXP gvp = gridStateElement(dd, GSS_VP);
    /* NOTE that the R code has already checked that .grid.viewport$parent
     * is non-NULL
     */
    PROTECT(newvp = getListElement(gvp, "parent"));
    if (LOGICAL(last)[0]) {
	double devWidthCM, devHeightCM;
	/* Get the current device size 
	 */
	getDeviceSize(dd, &devWidthCM, &devHeightCM);
	if (deviceChanged(devWidthCM, devHeightCM, dd))
	    calcViewportTransform(newvp, viewportParent(newvp), 1, dd);
    }
    /* Set the clipping region to the parent's cur.clip
     */
    parentClip = viewportCurClip(newvp);
    xx1 = REAL(parentClip)[0];
    yy1 = REAL(parentClip)[1];
    xx2 = REAL(parentClip)[2];
    yy2 = REAL(parentClip)[3];
    GESetClip(xx1, yy1, xx2, yy2, dd);
	    /* This is a VERY short term fix to avoid mucking
	     * with the core graphics during feature freeze
	     * It should be removed post R 1.4 release
	     */
	    dd->dev->clipLeft = fmin2(xx1, xx2);
	    dd->dev->clipRight = fmax2(xx1, xx2);
	    dd->dev->clipTop = fmax2(yy1, yy2);
	    dd->dev->clipBottom = fmin2(yy1, yy2); 
    /* Set the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    setGridStateElement(dd, GSS_VP, newvp);
    UNPROTECT(1);
    return R_NilValue;
}

SEXP L_getDisplayList() 
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_DL);
}

SEXP L_setDisplayList(SEXP dl) 
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    setGridStateElement(dd, GSS_DL, dl);
    return R_NilValue;
}

/* Add an element to the display list at the current location
 * Location is maintained in R code
 */
SEXP L_setDLelt(SEXP value)
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    SEXP dl;
    PROTECT(dl = gridStateElement(dd, GSS_DL));
    SET_VECTOR_ELT(dl, INTEGER(gridStateElement(dd, GSS_DLINDEX))[0], value);
    UNPROTECT(1);
    return R_NilValue;
}

SEXP L_getDLindex()
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_DLINDEX);
}

SEXP L_setDLindex(SEXP index)
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    setGridStateElement(dd, GSS_DLINDEX, index);
    return R_NilValue;
}

SEXP L_getDLon()
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_DLON);
}

SEXP L_setDLon(SEXP value)
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    setGridStateElement(dd, GSS_DLON, value);
    return R_NilValue;
}

SEXP L_currentGPar()
{
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    return gridStateElement(dd, GSS_GPAR);
}

SEXP L_newpagerecording(SEXP ask)
{
    GEDevDesc *dd = getDevice();
    if (LOGICAL(ask)[0]) {
	NewFrameConfirm();
    }
    GEinitDisplayList(dd);
    return R_NilValue;
}

SEXP L_newpage()
{
    GEDevDesc *dd = getDevice();
    if (!LOGICAL(gridStateElement(dd, GSS_GRIDDEVICE))[0]) 
	dirtyGridDevice(dd);
    else {
	SEXP currentgp = gridStateElement(dd, GSS_GPAR);
	SEXP fill = gpFillSXP(currentgp);
	if (isNull(fill))
	    GENewPage(NA_INTEGER, gpGamma(currentgp, 0), dd);
	else
	    GENewPage(RGBpar(fill, 0), gpGamma(currentgp, 0), dd);
    }
    return R_NilValue;
}

SEXP L_initGPar()
{
    GEDevDesc *dd = getDevice();
    initGPar(dd);
    return R_NilValue;
}

SEXP L_initViewportStack()
{
    GEDevDesc *dd = getDevice();
    initVP(dd);
    return R_NilValue;
}

SEXP L_initDisplayList()
{
    GEDevDesc *dd = getDevice();
    initDL(dd);
    return R_NilValue;
}

void getViewportTransform(SEXP currentvp, 
			  GEDevDesc *dd, 
			  double *vpWidthCM, double *vpHeightCM,
			  LTransform transform, double *rotationAngle) 
{
    int i, j;
    double devWidthCM, devHeightCM;
    getDeviceSize((dd), &devWidthCM, &devHeightCM) ;
    if (deviceChanged(devWidthCM, devHeightCM, dd)) {
	/* IF the device has changed, recalculate the viewport transform
	 */
	calcViewportTransform(currentvp, viewportParent(currentvp), 1, dd); 
    }
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    transform[i][j] = 
		REAL(viewportCurrentTransform(currentvp))[i + 3*j];
    *rotationAngle = REAL(viewportCurrentRotation(currentvp))[0];
    *vpWidthCM = REAL(viewportCurrentWidthCM(currentvp))[0];
    *vpHeightCM = REAL(viewportCurrentHeightCM(currentvp))[0];
}


/***************************
 * CONVERSION FUNCTIONS
 ***************************
 */

/* Just do a convert-to-native function for now
 * what = 0 means x, 1 means y, 2 means width, 3 means height
 * NOTE that we are only converting relative to the viewport
 * NOT relative to the device
 */
SEXP L_convertToNative(SEXP x, SEXP what) 
{
    int i, nx;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP result, currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x);
    PROTECT(result = allocVector(REALSXP, nx));
    switch (INTEGER(what)[0]) {
    case 0:
	for (i=0; i<nx; i++)  
	    REAL(result)[i] = transformXtoNative(x, i, vpc, 
						 gpFontFamily(currentgp, i),
						 gpFont(currentgp, i),
						 gpFontSize(currentgp, i), 
						 gpLineHeight(currentgp, i),
						 vpWidthCM, vpHeightCM,
						 dd);
	break;
    case 1:	
	for (i=0; i<nx; i++)  
	    REAL(result)[i] = transformYtoNative(x, i, vpc, 
						 gpFontFamily(currentgp, i),
						 gpFont(currentgp, i),
						 gpFontSize(currentgp, i), 
						 gpLineHeight(currentgp, i),
						 vpWidthCM, vpHeightCM,
						 dd);
	break;
    case 2:
	for (i=0; i<nx; i++)  
	    REAL(result)[i] = transformWidthtoNative(x, i, vpc, 
						     gpFontFamily(currentgp, i),
						     gpFont(currentgp, i),
						     gpFontSize(currentgp, i), 
						     gpLineHeight(currentgp, i),
						     vpWidthCM, vpHeightCM,
						     dd);
	break;
    case 3:
	for (i=0; i<nx; i++)  
	    REAL(result)[i] = transformHeighttoNative(x, i, vpc, 
						      gpFontFamily(currentgp, i),
						      gpFont(currentgp, i),
						      gpFontSize(currentgp, i), 
						      gpLineHeight(currentgp, i),
						      vpWidthCM, vpHeightCM,
						      dd);
	break;
    }
    UNPROTECT(1);
    return result;
    
}

/***************************
 * DRAWING PRIMITIVES
 ***************************
 */

SEXP L_moveTo(SEXP x, SEXP y)
{    
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP devloc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    PROTECT(devloc = gridStateElement(dd, GSS_CURRLOC));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    /* Convert the x and y values to CM locations */
    transformLocn(x, y, 0, vpc, 
		  gpFontFamily(currentgp, 0),
		  gpFont(currentgp, 0),
		  gpFontSize(currentgp, 0), gpLineHeight(currentgp, 0),
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    /* The graphics engine only takes device coordinates
     */
    xx = toDeviceX(xx, GE_INCHES, dd);
    yy = toDeviceY(yy, GE_INCHES, dd);
    REAL(devloc)[0] = xx;
    REAL(devloc)[1] = yy;
    UNPROTECT(1);
    return R_NilValue;
}

SEXP L_lineTo(SEXP x, SEXP y)
{
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP devloc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    PROTECT(devloc = gridStateElement(dd, GSS_CURRLOC));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    /* Convert the x and y values to CM locations */
    transformLocn(x, y, 0, vpc,  
		  gpFontFamily(currentgp, 0),
		  gpFont(currentgp, 0),
		  gpFontSize(currentgp, 0), gpLineHeight(currentgp, 0),
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    /* The graphics engine only takes device coordinates
     */
    xx = toDeviceX(xx, GE_INCHES, dd);
    yy = toDeviceY(yy, GE_INCHES, dd);
    GEMode(1, dd);
    GELine(REAL(devloc)[0], REAL(devloc)[1], xx, yy, 
	   gpCol(currentgp, 0), gpGamma(currentgp, 0),
	   gpLineType(currentgp, 0), gpLineWidth(currentgp, 0), 
	   dd);
    GEMode(0, dd);
    REAL(devloc)[0] = xx;
    REAL(devloc)[1] = yy;
    UNPROTECT(1);
    return R_NilValue;
}

/* We are assuming here that the R code has checked that x and y 
 * are unit objects and that vp is a viewport
 */
SEXP L_lines(SEXP x, SEXP y) 
{
    int i, nx;
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    /* Convert the x and y values to CM locations */
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, 0),
		      gpFontSize(currentgp, 0), gpLineHeight(currentgp, 0),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
	/* The graphics engine only takes device coordinates
	 */
	xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
    }
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    GEPolyline(nx, xx, yy, 
	       gpCol(currentgp, 0), gpGamma(currentgp, 0),
	       gpLineType(currentgp, 0), gpLineWidth(currentgp, 0), 
	       dd);
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_segments(SEXP x0, SEXP y0, SEXP x1, SEXP y1) 
{
    int i, nx0, ny0, nx1, ny1, maxn;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    maxn = nx0 = unitLength(x0); 
    ny0 = unitLength(y0);
    nx1 = unitLength(x1);
    ny1 = unitLength(y1);
    if (ny0 > maxn)
	maxn = ny0;
    if (nx1 > maxn)
	maxn = nx1;
    if (ny1 > maxn)
	maxn = ny1;
    /* Convert the x and y values to INCHES locations */
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    for (i=0; i<maxn; i++) {
	double xx0, yy0, xx1, yy1;
	transformLocn(x0, y0, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx0, &yy0);
	transformLocn(x1, y1, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx1, &yy1);
	/* The graphics engine only takes device coordinates
	 */
	xx0 = toDeviceX(xx0, GE_INCHES, dd);
	yy0 = toDeviceY(yy0, GE_INCHES, dd);
	xx1 = toDeviceX(xx1, GE_INCHES, dd);
	yy1 = toDeviceY(yy1, GE_INCHES, dd);
	GELine(xx0, yy0, xx1, yy1, 
	       gpCol(currentgp, i), gpGamma(currentgp, i),
	       gpLineType(currentgp, i), gpLineWidth(currentgp, i),
	       dd);
    }
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_polygon(SEXP x, SEXP y)
{
    int i, nx;
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    /* Convert the x and y values to CM locations */
    xx = (double *) R_alloc(nx + 1, sizeof(double));
    yy = (double *) R_alloc(nx + 1, sizeof(double));
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, 0),
		      gpFontSize(currentgp, 0), gpLineHeight(currentgp, 0),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
	/* The graphics engine only takes device coordinates
	 */
	xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
    }
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    GEPolygon(nx, xx, yy, 
	      gpCol(currentgp, 0), gpFill(currentgp, 0), gpGamma(currentgp, 0),
	      gpLineType(currentgp, 0), gpLineWidth(currentgp, 0),
	      dd);
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_circle(SEXP x, SEXP y, SEXP r)
{
    int i, nx, nr;
    double xx, yy, rr1, rr2, rr;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    nr = unitLength(r);
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &xx, &yy);
	/* These two will give the same answer unless r is in "native",
	 * "npc", or some other relative units;  in those cases, just
	 * take the smaller of the two values.
	 */
	rr1 = transformWidthtoINCHES(r, i % nr, vpc, 
				     gpFontFamily(currentgp, i),
				     gpFont(currentgp, i),
				     gpFontSize(currentgp, i), 
				     gpLineHeight(currentgp, i),
				     vpWidthCM, vpHeightCM,
				     dd);
	rr2 = transformHeighttoINCHES(r, i % nr, vpc, 
				      gpFontFamily(currentgp, i),
				      gpFont(currentgp, i),
				      gpFontSize(currentgp, i), 
				      gpLineHeight(currentgp, i),
				      vpWidthCM, vpHeightCM,
				      dd);
	rr = fmin2(rr1, rr2);
	rr = toDeviceWidth(rr, GE_INCHES, dd);
	/* The graphics engine only takes device coordinates
	 */
	xx = toDeviceX(xx, GE_INCHES, dd);
	yy = toDeviceY(yy, GE_INCHES, dd);
	GECircle(xx, yy, rr, 
		gpCol(currentgp, i), gpFill(currentgp, i), gpGamma(currentgp, i),
		gpLineType(currentgp, i), gpLineWidth(currentgp, i),
		dd);
    }
    GEMode(0, dd);
    return R_NilValue;
}

/* We are assuming here that the R code has checked that 
 * x, y, w, and h are all unit objects and that vp is a viewport
 */
SEXP L_rect(SEXP x, SEXP y, SEXP w, SEXP h, SEXP just) 
{
    double xx, yy, ww, hh;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    int i, nx;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    /* FIXME:  Need to check for x, y, w, h all same length
     */
    nx = unitLength(x); 
    GEMode(1, dd);
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), 
		      gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &xx, &yy);
	ww = transformWidthtoINCHES(w, i, vpc, 
				    gpFontFamily(currentgp, i),
				    gpFont(currentgp, i),
				    gpFontSize(currentgp, i), 
				    gpLineHeight(currentgp, i),
				    vpWidthCM, vpHeightCM,
				    dd);
	hh = transformHeighttoINCHES(h, i, vpc, 
				     gpFontFamily(currentgp, i),
				     gpFont(currentgp, i),
				     gpFontSize(currentgp, i), 
				     gpLineHeight(currentgp, i),
				     vpWidthCM, vpHeightCM,
				     dd);
	/* FIXME:  Need to check for NaN's and NA's
	 */
	/* If the total rotation angle is zero then we can draw a 
	 * rectangle as the devices understand rectangles
	 * Otherwise we have to draw a polygon equivalent.
	 */
	if (rotationAngle == 0) {
	    xx = justifyX(xx, ww, INTEGER(just)[0]);
	    yy = justifyY(yy, hh, INTEGER(just)[1]);
	    /* The graphics engine only takes device coordinates
	     */
	    xx = toDeviceX(xx, GE_INCHES, dd);
	    yy = toDeviceY(yy, GE_INCHES, dd);
	    ww = toDeviceWidth(ww, GE_INCHES, dd);
	    hh = toDeviceHeight(hh, GE_INCHES, dd);
	    GERect(xx, yy, xx + ww, yy + hh, 
		   gpCol(currentgp, i), gpFill(currentgp, i), gpGamma(currentgp, i),
		   gpLineType(currentgp, i), gpLineWidth(currentgp, i),
		   dd);
	} else {
	    /* We have to do a little bit of work to figure out where the 
	     * corners of the rectangle are.
	     */
	    double xxx[5], yyy[5], xadj, yadj;
	    double dw, dh;
	    SEXP temp = unit(0, L_INCHES);
	    SEXP www, hhh;
	    /* Find bottom-left location */
	    justification(ww, hh, INTEGER(just)[0], INTEGER(just)[1], 
			  &xadj, &yadj);
	    www = unit(xadj, L_INCHES);
	    hhh = unit(yadj, L_INCHES);
	    transformDimn(www, hhh, 0, vpc, 
			  gpFontFamily(currentgp, i),
			  gpFont(currentgp, i),
			  gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
			  vpWidthCM, vpHeightCM,
			  dd, rotationAngle,
			  &dw, &dh);
	    xxx[0] = xx + dw;
	    yyy[0] = yy + dh;
	    /* Find top-left location */
	    www = temp;
	    hhh = unit(hh, L_INCHES);
	    transformDimn(www, hhh, 0, vpc, 
			  gpFontFamily(currentgp, i),
			  gpFont(currentgp, i),
			  gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
			  vpWidthCM, vpHeightCM,
			  dd, rotationAngle,
			  &dw, &dh);
	    xxx[1] = xxx[0] + dw;
	    yyy[1] = yyy[0] + dh;
	    /* Find top-right location */
	    www = unit(ww, L_INCHES);
	    hhh = unit(hh, L_INCHES);
	    transformDimn(www, hhh, 0, vpc, 
			  gpFontFamily(currentgp, i),
			  gpFont(currentgp, i),
			  gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
			  vpWidthCM, vpHeightCM,
			  dd, rotationAngle,
			  &dw, &dh);
	    xxx[2] = xxx[0] + dw;
	    yyy[2] = yyy[0] + dh;
	    /* Find bottom-right location */
	    www = unit(ww, L_INCHES);
	    hhh = temp;
	    transformDimn(www, hhh, 0, vpc, 
			  gpFontFamily(currentgp, i),
			  gpFont(currentgp, i),
			  gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
			  vpWidthCM, vpHeightCM,
			  dd, rotationAngle,
			  &dw, &dh);
	    xxx[3] = xxx[0] + dw;
	    yyy[3] = yyy[0] + dh;
	    /* The graphics engine only takes device coordinates
	     */
	    xxx[0] = toDeviceX(xxx[0], GE_INCHES, dd);
	    yyy[0] = toDeviceY(yyy[0], GE_INCHES, dd);
	    xxx[1] = toDeviceX(xxx[1], GE_INCHES, dd);
	    yyy[1] = toDeviceY(yyy[1], GE_INCHES, dd);
	    xxx[2] = toDeviceX(xxx[2], GE_INCHES, dd);
	    yyy[2] = toDeviceY(yyy[2], GE_INCHES, dd);
	    xxx[3] = toDeviceX(xxx[3], GE_INCHES, dd);
	    yyy[3] = toDeviceY(yyy[3], GE_INCHES, dd);
	    /* Close the polygon */
	    xxx[4] = xxx[0];
	    yyy[4] = yyy[0];
	    /* Do separate fill and border to avoid border being 
	     * drawn on clipping boundary when there is a fill
	     */
	    GEPolygon(5, xxx, yyy, 
		      NA_INTEGER, gpFill(currentgp, i), gpGamma(currentgp, i),
		      gpLineType(currentgp, i), gpLineWidth(currentgp, i),
		      dd);
	    GEPolygon(5, xxx, yyy, 
		      gpCol(currentgp, i), NA_INTEGER, gpGamma(currentgp, i),
		      gpLineType(currentgp, i), gpLineWidth(currentgp, i),
		      dd);
	}
    }
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_text(SEXP label, SEXP x, SEXP y, SEXP just, 
	    SEXP rot, SEXP checkOverlap)
{
    int i, nx, ny;
    double *xx, *yy;
    double hjust, vjust;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    SEXP txt;
    /* Bounding rectangles for checking overlapping
     */
    LRect *bounds;
    int numBounds = 0;
    int overlapChecking = LOGICAL(checkOverlap)[0];
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    ny = unitLength(y);
    if (ny > nx) 
	nx = ny;
    hjust = convertJust(INTEGER(just)[0]);
    vjust = convertJust(INTEGER(just)[1]);
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
    }
    /* The label can be a string or an expression
     */
    PROTECT(txt = label);
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    if (overlapChecking) {
	bounds = (LRect *) R_alloc(nx, sizeof(LRect));
    }
    GEMode(1, dd);
    for (i=0; i<nx; i++) {
	int doDrawing = 1;
	if (overlapChecking) {
	    int j = 0;
	    LRect trect;
	    textRect(xx[i], yy[i], txt, i,
		     gpFontFamily(currentgp, i),
		     gpFont(currentgp, i), 
		     gpLineHeight(currentgp, i),
		     1, gpFontSize(currentgp, i),
		     hjust, vjust, 
		     numeric(rot, i % LENGTH(rot)) + rotationAngle, 
		     dd, &trect);
	    while (doDrawing && (j < numBounds)) 
		if (intersect(trect, bounds[j++]))
		    doDrawing = 0;
	    if (doDrawing) {
		copyRect(trect, &(bounds[numBounds]));
		numBounds++;
	    }
	}
	if (doDrawing) {
	    /* FIXME:  Need to check for NaN's and NA's
	     */
	    /* The graphics engine only takes device coordinates
	     */
	    xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	    yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
	    if (isExpression(txt))
		GEMathText(xx[i], yy[i],
			   VECTOR_ELT(txt, i % LENGTH(txt)),
			   hjust, vjust, 
			   numeric(rot, i % LENGTH(rot)) + rotationAngle, 
			   gpCol(currentgp, i), gpGamma(currentgp, i), 
			   gpFont(currentgp, i),
			   gpCex(currentgp, i), gpFontSize(currentgp, i),
			   dd);
	    else
		GEText(xx[i], yy[i], 
		       CHAR(STRING_ELT(txt, i % LENGTH(txt))), 
		       hjust, vjust, 
		       numeric(rot, i % LENGTH(rot)) + rotationAngle, 
		       gpCol(currentgp, i), gpGamma(currentgp, i), 
		       gpFontFamily(currentgp, i), gpFont(currentgp, i),
		       gpLineHeight(currentgp, i),
		       gpCex(currentgp, i), gpFontSize(currentgp, i),
		       dd);
	}
    }
    GEMode(0, dd);
    UNPROTECT(1);
    return R_NilValue;    
}

SEXP L_points(SEXP x, SEXP y, SEXP pch, SEXP size)
{
    int i, nx, npch;
    /*    double *xx, *yy;*/
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    double symbolSize;
    LViewportContext vpc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    GEDevDesc *dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    npch = LENGTH(pch);
    /* Convert the x and y values to CM locations */
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    for (i=0; i<nx; i++) {
	transformLocn(x, y, i, vpc, 
		      gpFontFamily(currentgp, i),
		      gpFont(currentgp, i),
		      gpFontSize(currentgp, i), gpLineHeight(currentgp, i),
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
	/* The graphics engine only takes device coordinates
	 */
	xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
    }
    GEMode(1, dd);
    for (i=0; i<nx; i++)
	if (R_FINITE(xx[i]) && R_FINITE(yy[i])) {
	    /* FIXME:  The symbols will not respond to viewport
	     * rotations !!!
	     */
	    int ipch;
	    symbolSize = transformWidthtoINCHES(size, i, vpc, 
						gpFontFamily(currentgp, i),
						gpFont(currentgp, i),
						gpFontSize(currentgp, i), 
						gpLineHeight(currentgp, i),
						vpWidthCM, vpHeightCM, dd);
	    /* The graphics engine only takes device coordinates
	     */
	    symbolSize = toDeviceWidth(symbolSize, GE_INCHES, dd);
	    if (isString(pch))
		ipch = CHAR(STRING_ELT(pch, i % npch))[0];
	    else
		ipch = INTEGER(pch)[i % npch];
	    GESymbol(xx[i], yy[i], ipch, symbolSize,
		     gpCol(currentgp, i), gpFill(currentgp, i), gpGamma(currentgp, i),
		     gpLineType(currentgp, i), gpLineWidth(currentgp, i),
		     gpFont(currentgp, i), gpCex(currentgp, i), 
		     gpFontSize(currentgp, i),
		     dd);
	}
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_pretty(SEXP scale) {
    double min = numeric(scale, 0);
    double max = numeric(scale, 1);
    /* FIXME:  This is just a dummy pointer because we do not have
     * log scales.  This will cause death and destruction if it is 
     * not addressed when log scales are added !
     */
    double *usr = NULL;
    double axp[3];
    /* FIXME:  Default preferred number of ticks hard coded ! */
    int n = 5;
    GEPretty(&min, &max, &n);
    axp[0] = min;
    axp[1] = max;
    axp[2] = n;
    /* FIXME:  "log" flag hard-coded to FALSE because we do not
     * have log scales yet
     */
    return CreateAtVector(axp, usr, n, FALSE);
}






