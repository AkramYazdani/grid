#include "grid.h"

/* IMPORTANT NOTE:
 * The extensive use of L or L_ prefixes dates back to when this 
 * package used to be called "lattice"
 */

/* Global record of the transform for the current viewport 
 * (i.e., the viewport that drawing occurs in by default
 * (i.e., unless a temporary viewport has been specified))
 */
LTransform L_viewportTransform;

/* Global record of the total angle of rotation for the current viewport
 */
double L_rotationAngle;

/* Global record of the size in CM of the current viewport
 */
double L_viewportWidthCM;
double L_viewportHeightCM;

/* Global LViewportContext
 * All drawing occurs within this context.
 * Set permanently by L_setviewport.
 * Can also be set temporarily by any other L_* function, but those
 * functions are responsible for restoring the original setting.
 */
LViewportContext L_viewportContext;

/* Global device dimensions
 * 
 */
double L_devWidthCM = 0;
double L_devHeightCM = 0;
/* FIXME:  Need to query device for this
 */
double devPS = 10;

void getDeviceSize(DevDesc *dd, double *devWidthCM, double *devHeightCM) 
{
    *devWidthCM = GConvertXUnits(1.0, NDC, INCHES, dd)*2.54;
    *devHeightCM = GConvertYUnits(1.0, NDC, INCHES, dd)*2.54;
}

Rboolean deviceChanged(double devWidthCM, double devHeightCM)
{
    Rboolean result = FALSE;
    if (fabs(L_devWidthCM - devWidthCM) > DBL_EPSILON) {
	result = TRUE;
	L_devWidthCM = devWidthCM;
    }
    if (fabs(L_devHeightCM - devHeightCM) > DBL_EPSILON) {
	result = TRUE;
	L_devHeightCM = devHeightCM;
    }
    return result;
}

SEXP L_setviewport(SEXP vp)
{
    double devWidthCM, devHeightCM;
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
    /* Get the current device size 
     */
    getDeviceSize(dd, &devWidthCM, &devHeightCM);
    viewportTransform(vp, viewportParent(vp), dd,
		      &L_viewportWidthCM, &L_viewportHeightCM,
		      L_viewportTransform, &L_rotationAngle);
    fillViewportContextFromViewport(vp, &L_viewportContext);
    return R_NilValue;
}

/* FIXME:  GNewPlot runs in two different modes, depending on whether
 * it is being recorded on the display list or being replayed from the
 * the display list (it takes a boolean argument to specify which mode).
 * Unfortunately, since this is non-internal code, I cannot tell 
 * whether I am being recorded on the display list or being 
 * replayed from the display list.
 * SO ... the L_newpagerecording function is a copy of the bit of
 * GNewPlot that gets run when recording on the display list
 * and I only .Call this function (so that it never gets replayed
 * from the display list) 
 * AND ... the L_newpage function just calls GNewPlot with the 
 * boolean set to FALSE (i.e., it does the stuff that GNewPlot does
 * when it is recording to the display list) and I call this with
 * .Call.graphics so that it gets replayed from the display list.
 * NOTE that this only works because lattice graphics only ever 
 * has one plot - par(mfrow=c(1,1)) - in base R graphics terms.
 * Specifically, dd->gp.lastFigure = 1
 * NOTE that this does not do any plot history recording
 * Ultimately, this could be fixed by either (i) making lattice 
 * graphics internal code or (ii) splitting GNewPlot into bits
 * in graphics.c so that I don't have to have a copy of the code here.
 */
SEXP L_newpagerecording(SEXP ask)
{
    DevDesc *dd = CurrentDevice();
    if (LOGICAL(ask)[0]) {
	NewFrameConfirm();
	if (NoDevices())
	    error("attempt to plot on null device");
	else
	    dd = CurrentDevice();
    }
    initDisplayList(dd);
    return R_NilValue;
}

SEXP L_newpage()
{
    /* FALSE means GNewPlot does not think it is recording
     * i.e., it thinks it is being replayed from the display list.
     */
    DevDesc *dd = GNewPlot(FALSE);
    /* Fool base R graphics into thinking that plot.new() has been called 
     */
    GSetState(1, dd);
    return R_NilValue;
}

void getViewportTransform(SEXP currentvp, 
			  DevDesc *dd, 
			  double *vpWidthCM, double *vpHeightCM,
			  LTransform transform, double *rotationAngle) 
{
    double devWidthCM, devHeightCM;
    getDeviceSize(dd, &devWidthCM, &devHeightCM) ;
    if (deviceChanged(devWidthCM, devHeightCM)) {
	/* IF the device has changed, recalculate the viewport transform
	 */
	viewportTransform(currentvp, viewportParent(currentvp), dd, 
			  vpWidthCM, vpHeightCM, 
			  transform, rotationAngle);
	/* AND save the default viewport transform
	 */
	copyTransform(transform, L_viewportTransform);
	L_rotationAngle = *rotationAngle;
	L_viewportWidthCM = *vpWidthCM;
	L_viewportHeightCM = *vpHeightCM;
    }
    else {
	/* ELSE just reuse the normalised default viewport 
	 */
	copyTransform(L_viewportTransform, transform);
	*rotationAngle = L_rotationAngle;
	*vpWidthCM = L_viewportWidthCM;
	*vpHeightCM = L_viewportHeightCM;
    }
}

void getViewportContext(SEXP vp, LViewportContext *vpc)
{
    /* NOTE that this just copies the global viewport context
     * which contains information about fontsize, which did not
     * come from a viewport.  
     * If this changes so that information is obtained from a 
     * viewport then we will need to be given information about
     * fontsize as well.
     */
    copyViewportContext(L_viewportContext, vpc);
}
    
/* We are assuming here that the R code has checked that x and y 
 * are unit objects and that vp is a viewport
 */
SEXP L_lines(SEXP x, SEXP y, SEXP currentvp) 
{
    int i, nx;
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
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
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
    }
    /* Indicate to the device that drawing is about to begin */
    GMode(1, dd);
    /* FIXME:  Need to check for NaN's and NA's
     */
    GPolyline(nx, xx, yy, INCHES, dd);
    /* Indicate to the device that drawing has finished */
    GMode(0, dd);
    return R_NilValue;
}

SEXP L_segments(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP currentvp) 
{
    int i, nx0, ny0, nx1, ny1, maxn;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
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
    /* Indicate to the device that drawing is about to begin */
    GMode(1, dd);
    /* Convert the x and y values to INCHES locations */
    /* FIXME:  Need to check for NaN's and NA's
     */
    for (i=0; i<maxn; i++) {
	double xx0, yy0, xx1, yy1;
	transformLocn(x0, y0, i, vpc,
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx0, &yy0);
	transformLocn(x1, y1, i, vpc,
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx1, &yy1);
	GLine(xx0, yy0, xx1, yy1, INCHES, dd);
    }
    /* Indicate to the device that drawing has finished */
    GMode(0, dd);
    return R_NilValue;
}

/* We are assuming here that the R code has checked that 
 * x, y, w, and h are all unit objects and that vp is a viewport
 */
SEXP L_rect(SEXP x, SEXP y, SEXP w, SEXP h,
	    SEXP just, SEXP border, SEXP fill, 
	    SEXP currentvp) 
{
    double xx, yy, ww, hh;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    transformLocn(x, y, 0, vpc,
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    transformDimn(w, h, 0, vpc,
		  vpWidthCM, vpHeightCM,
		  dd,
		  rotationAngle,
		  &ww, &hh);
    xx = justifyX(xx, ww, INTEGER(just)[0]);
    yy = justifyY(yy, hh, INTEGER(just)[1]);
    /* FIXME: Force a clip - I think there is a bug in GRect which means 
     * that GClip is not called when the rect is completely within 
     * the clipping region.  This is masked in normal graphics
     * by GNewPlot which does a GForceClip.
     * In lattice graphics there is no GNewPlot so clipping doesn't
     * happen elsewhere and the GRect bug is exposed.
     * In other words, this can be removed once the GRect bug is fixed.
     * To see the bug in action, comment out the line below and 
     * try lshow.viewport(viewport()) and resize the window.
     * Why do we want to call GClip ?  Well, so that it calls X11_clip
     * which will update the clipping region for the device, which we
     * need because the device size has changed.  
     * Having said that, maybe the problem is not at GRect (because 
     * GClip will only call X11_clip if par(xpd) has changed !) but
     * rather with the fact that lattice graphics does not have an
     * euivalent of GNewPlot (yet anyway) where a GForceClip can be
     * conveniently placed.
     */
    GForceClip(dd);
    /* Indicate to the device that drawing is about to begin */
    GMode(1, dd);
    /* FIXME:  Need to check for NaN's and NA's
     */
    /* If the total rotation angle is zero then we can draw a 
     * rectangle as the devices understand rectangles
     * Otherwise we have to draw a polygon equivalent.
     */
    if (rotationAngle == 0) {
	GRect(xx, yy, xx + ww, yy + hh, INCHES, 
	      INTEGER(FixupCol(fill, NA_INTEGER))[0], 
	      INTEGER(FixupCol(border, NA_INTEGER))[0], dd);
    } else {
	/* We have the bottom-left and top-right corners but we have to
	 * do a little bit of work to figure out where the other
	 * corners of the rectangle are.
	 */
	double xxx[5], yyy[5];
	double w1, h1, w2, h2;
	SEXP temp = unit(0, L_INCHES);
	transformDimn(w, temp, 0, vpc,
		      vpWidthCM, vpHeightCM,
		      dd,
		      rotationAngle,
		      &w1, &h1);
	transformDimn(temp, h, 0, vpc,
		      vpWidthCM, vpHeightCM,
		      dd,
		      rotationAngle,
		      &w2, &h2);
	xxx[0] = xx;       yyy[0] = yy;
	xxx[1] = xx + w1;  yyy[1] = yy + h1;
	xxx[2] = xx + ww;  yyy[2] = yy + hh;
	xxx[3] = xx + w2;  yyy[3] = yy + h2;
	xxx[4] = xx;       yyy[4] = yy;
	/* Do separate fill and border to avoid border being 
	 * drawn on clipping boundary when there is a fill
	 */
	if (!isNull(fill))
	    GPolygon(5, xxx, yyy, INCHES, 
		     INTEGER(FixupCol(fill, NA_INTEGER))[0], 
		     NA_INTEGER, dd);
	if (!isNull(border))
	    GPolygon(5, xxx, yyy, INCHES, 
		     NA_INTEGER,
		     INTEGER(FixupCol(border, NA_INTEGER))[0], dd);
    }
    /* Indicate to the device that drawing has finished */
    GMode(0, dd);
    return R_NilValue;
}

SEXP L_text(SEXP label, SEXP x, SEXP y, SEXP just, 
	    SEXP rot, SEXP checkOverlap, SEXP currentvp)
{
    int i, nx, ny;
    double *xx, *yy;
    double hjust, vjust;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    /* Bounding rectangles for checking overlapping
     */
    LRect *bounds;
    int numBounds = 0;
    int overlapChecking = LOGICAL(checkOverlap)[0];
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
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
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
    }
    if (overlapChecking) {
	bounds = (LRect *) R_alloc(nx, sizeof(LRect));
    }
    /* Indicate to the device that drawing is about to begin */
    GMode(1, dd);
    for (i=0; i<nx; i++) {
	int doDrawing = 1;
	if (overlapChecking) {
	    int j = 0;
	    LRect trect;
	    textRect(xx[i], yy[i], 
		     CHAR(STRING_ELT(label, i % LENGTH(label))), 
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
	if (doDrawing)
	    /* FIXME:  Need to check for NaN's and NA's
	     */
	    GText(xx[i], yy[i], INCHES, 
		  CHAR(STRING_ELT(label, i % LENGTH(label))), 
		  hjust, vjust, 
		  numeric(rot, i % LENGTH(rot)) + rotationAngle, dd);
    }
    /* Indicate to the device that drawing has finished */
    GMode(0, dd);
    return R_NilValue;    
}

void myGSymbol(double x, double y, int coords, int pch, int col, int bg,
	       double GSTR_0, DevDesc *dd);

SEXP L_points(SEXP x, SEXP y, SEXP pch, SEXP size, SEXP col, SEXP bg,
	      SEXP currentvp)
{
    int i, nx;
    /*    double *xx, *yy;*/
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    double symbolSize;
    LViewportContext vpc;
    LTransform transform;
    /* Get the current device 
     */
    DevDesc *dd = CurrentDevice();
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
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
    }
    symbolSize = transformWidthtoINCHES(size, 0, vpc, 
					vpWidthCM, vpHeightCM, dd);
    /* Indicate to the device that drawing is about to begin */
    GMode(1, dd);
    for (i=0; i<nx; i++)
	if (R_FINITE(xx[i]) && R_FINITE(yy[i]))
	    /* FIXME:  Does not handle vector pch yet */
	    /* FIXME:  The symbols will not respond to viewport
	     * rotations !!!
	     */
	    myGSymbol(xx[i], yy[i], INCHES, INTEGER(FixupPch(pch, 1))[0], 
		      INTEGER(FixupCol(col, 1))[0], 
		      INTEGER(FixupCol(bg, 1))[0],
		      symbolSize,
		      dd);
    /* Indicate to the device that drawing has finished */
    GMode(0, dd);
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
    GPretty(&min, &max, &n);
    axp[0] = min;
    axp[1] = max;
    axp[2] = n;
    /* FIXME:  "log" flag hard-coded to FALSE because we do not
     * have log scales yet
     */
    return CreateAtVector(axp, usr, n, FALSE);
}

#define SMALL	0.25
#define RADIUS	0.375 
#define SQRC	0.88622692545275801364		/* sqrt(pi / 4) */
#define DMDC	1.25331413731550025119		/* sqrt(pi / 4) * sqrt(2) */
#define TRC0	1.55512030155621416073		/* sqrt(4 * pi/(3 * sqrt(3))) */
#define TRC1	1.34677368708859836060		/* TRC0 * sqrt(3) / 2 */
#define TRC2	0.77756015077810708036		/* TRC0 / 2 */
#define CMAG	1.0				/* Circle magnifier, now defunct */
/* #define GSTR_0  dd->dp.cra[1] * 0.5 * dd->gp.ipr[0] * dd->gp.cex */ 
/* Draw one of the R special symbols. */
void myGSymbol(double x, double y, int coords, int pch, int col, int bg,
	       double GSTR_0, DevDesc *dd)
{
    double r, xc, yc;
    double xx[4], yy[4];
    char str[2];

    if(' ' <= pch && pch <= 255) {
	if (pch == '.') {
	    GConvert(&x, &y, coords, DEVICE, dd);
	    GRect(x-.5, y-.5, x+.5, y+.5, DEVICE, col, NA_INTEGER, dd);
	} else {
	    str[0] = pch;
	    str[1] = '\0';
	    GText(x, y, coords, str, NA_REAL, NA_REAL, 0., dd);
	}
    }
    else {
	switch(pch) {

	case 0: /* S square */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GRect(x-xc, y-xc, x+xc, y+xc, INCHES, NA_INTEGER,
		  col, dd);
	    break;

	case 1: /* S octahedron ( circle) */
	    xc = CMAG * RADIUS * GSTR_0;
	    GCircle(x, y, coords, xc, NA_INTEGER, col, dd);
	    break;

	case 2:	/* S triangle - point up */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    xc = TRC1 * xc;
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GPolygon(3, xx, yy, INCHES, NA_INTEGER, col, dd);
	    break;

	case 3: /* S plus */
	    xc = M_SQRT2*RADIUS*GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y, x+xc, y, INCHES, dd);
	    GLine(x, y-xc, x, y+xc, INCHES, dd);
	    break;

	case 4: /* S times */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y-xc, x+xc, y+xc, INCHES, dd);
	    GLine(x-xc, y+xc, x+xc, y-xc, INCHES, dd);
	    break;

	case 5: /* S diamond */
	    xc = M_SQRT2 * RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+xc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-xc;
	    GPolygon(4, xx, yy, INCHES, NA_INTEGER, col, dd);
	    break;

	case 6: /* S triangle - point down */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    xc = TRC1 * xc;
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GPolygon(3, xx, yy, INCHES, NA_INTEGER, col, dd);
	    break;

	case 7:	/* S square and times superimposed */
	    xc =  RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y-xc, x+xc, y+xc, INCHES, dd);
	    GLine(x-xc, y+xc, x+xc, y-xc, INCHES, dd);
	    GRect(x-xc, y-xc, x+xc, y+xc, INCHES, NA_INTEGER,
		  col, dd);
	    break;

	case 8: /* S plus and times superimposed */
	    xc =  RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y-xc, x+xc, y+xc, INCHES, dd);
	    GLine(x-xc, y+xc, x+xc, y-xc, INCHES, dd);
	    xc = M_SQRT2 * xc;
	    GLine(x-xc, y, x+xc, y, INCHES, dd);
	    GLine(x, y-xc, x, y+xc, INCHES, dd);
	    break;

	case 9: /* S diamond and plus superimposed */
	    xc = M_SQRT2 * RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x-xc; yy[0] = y;
	    xx[1] = x; yy[1] = y+xc;
	    xx[2] = x+xc; yy[2] = y;
	    xx[3] = x; yy[3] = y-xc;
	    GPolygon(4, xx, yy, INCHES, NA_INTEGER, col, dd);
	    GLine(x-xc, y, x+xc, y, INCHES, dd);
	    GLine(x, y-xc, x, y+xc, INCHES, dd);
	    break;

	case 10: /* S hexagon (circle) and plus superimposed */
	    xc = CMAG * RADIUS * GSTR_0;
	    GCircle(x, y, coords, xc, NA_INTEGER, col, dd);
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y, x+xc, y, INCHES, dd);
	    GLine(x, y-xc, x, y+xc, INCHES, dd);
	    break;

	case 11: /* S superimposed triangles */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    yc = 0.5 * (yc + r);
	    xc = TRC1 * xc;
	    xx[0] = x; yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GPolygon(3, xx, yy, INCHES, NA_INTEGER, col, dd);
	    xx[0] = x; yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GPolygon(3, xx, yy, INCHES, NA_INTEGER, col, dd);
	    break;

	case 12: /* S square and plus superimposed */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y, x+xc, y, INCHES, dd);
	    GLine(x, y-xc, x, y+xc, INCHES, dd);
	    GRect(x-xc, y-xc, x+xc, y+xc, INCHES,
		  NA_INTEGER, col, dd);
	    break;

	case 13: /* S octagon (circle) and times superimposed */
	    xc = CMAG * RADIUS * GSTR_0;
	    GCircle(x, y, coords, xc, NA_INTEGER, col, dd);
	    GConvert(&x, &y, coords, INCHES, dd);
	    GLine(x-xc, y-xc, x+xc, y+xc, INCHES, dd);
	    GLine(x-xc, y+xc, x+xc, y-xc, INCHES, dd);
	    break;

	case 14: /* S square and point-up triangle superimposed */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x; yy[0] = y+xc;
	    xx[1] = x+xc; yy[1] = y-xc;
	    xx[2] = x-xc; yy[2] = y-xc;
	    GPolygon(3, xx, yy, INCHES, NA_INTEGER, col, dd);
	    GRect(x-xc, y-xc, x+xc, y+xc, INCHES,
		  NA_INTEGER, col, dd);
	    break;

	case 15: /* S filled square */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x-xc; yy[0] = y-xc;
	    xx[1] = x+xc; yy[1] = y-xc;
	    xx[2] = x+xc; yy[2] = y+xc;
	    xx[3] = x-xc; yy[3] = y+xc;
	    GPolygon(4, xx, yy, INCHES, col,
		     NA_INTEGER, dd);
	    break;

	case 16: /* S filled octagon (circle) */
	    xc = RADIUS * GSTR_0;
	    GCircle(x, y, coords, xc, col, col, dd);
	    break;

	case 17: /* S filled point-up triangle */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    xc = TRC1 * xc;
	    xx[0] = x;	  yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GPolygon(3, xx, yy, INCHES, col,
		     NA_INTEGER, dd);
	    break;

	case 18: /* S filled diamond */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x;	  yy[0] = y-xc;
	    xx[1] = x+xc; yy[1] = y;
	    xx[2] = x;	  yy[2] = y+xc;
	    xx[3] = x-xc; yy[3] = y;
	    GPolygon(4, xx, yy, INCHES, col,
		     NA_INTEGER, dd);
	    break;

	case 19: /* R filled circle */
	    xc = RADIUS * GSTR_0;
	    GCircle(x, y, coords, xc, col, col, dd);
	    break;


	case 20: /* R `Dot' (small circle) */
	    xc = SMALL * GSTR_0;
	    GCircle(x, y, coords, xc, col, col, dd);
	    break;


	case 21: /* circles */
	    xc = RADIUS * CMAG * GSTR_0;
	    GCircle(x, y, coords, xc, bg, col, dd);
	    break;

	case  22: /* squares */
	    xc = RADIUS * SQRC * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    GRect(x-xc, y-xc, x+xc, y+xc, INCHES,
		  bg, col, dd);
	    break;

	case 23: /* diamonds */
	    xc = RADIUS * DMDC * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    xx[0] = x	  ; yy[0] = y-xc;
	    xx[1] = x+xc; yy[1] = y;
	    xx[2] = x	  ; yy[2] = y+xc;
	    xx[3] = x-xc; yy[3] = y;
	    GPolygon(4, xx, yy, INCHES,
		     bg, col, dd);
	    break;

	case 24: /* triangle (point up) */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    xc = TRC1 * xc;
	    xx[0] = x;	  yy[0] = y+r;
	    xx[1] = x+xc; yy[1] = y-yc;
	    xx[2] = x-xc; yy[2] = y-yc;
	    GPolygon(3, xx, yy, INCHES,
		     bg, col, dd);
	    break;

	case 25: /* triangle (point down) */
	    xc = RADIUS * GSTR_0;
	    GConvert(&x, &y, coords, INCHES, dd);
	    r = TRC0 * xc;
	    yc = TRC2 * xc;
	    xc = TRC1 * xc;
	    xx[0] = x;	  yy[0] = y-r;
	    xx[1] = x+xc; yy[1] = y+yc;
	    xx[2] = x-xc; yy[2] = y+yc;
	    GPolygon(3, xx, yy, INCHES,
		     bg, col, dd);
	    break;
	}
    }
}



