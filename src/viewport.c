#include "grid.h"

/* Some access methods for viewports */
SEXP viewportX(SEXP vp) {
    return getListElement(vp, "x");
}

SEXP viewportY(SEXP vp) {
    return getListElement(vp, "y");
}

SEXP viewportWidth(SEXP vp) {
    return getListElement(vp, "width");
}

SEXP viewportHeight(SEXP vp) {
    return getListElement(vp, "height");
}

double viewportFontSize(SEXP vp) {
    return numeric(getListElement(vp, "cur.fontsize"), 0);
}

double viewportLineHeight(SEXP vp) {
    return numeric(getListElement(vp, "cur.lineheight"), 0);
}

double viewportXScaleMin(SEXP vp) {
    return numeric(getListElement(vp, "xscale"), 0);
}

double viewportXScaleMax(SEXP vp) {
    return numeric(getListElement(vp, "xscale"), 1);
}

double viewportYScaleMin(SEXP vp) {
    return numeric(getListElement(vp, "yscale"), 0);
}

double viewportYScaleMax(SEXP vp) {
    return numeric(getListElement(vp, "yscale"), 1);
}

int viewportOrigin(SEXP vp) {
    return INTEGER(getListElement(vp, "valid.origin"))[0];
}

int viewportHJust(SEXP vp) {
    return INTEGER(getListElement(vp, "valid.just"))[0];
}

int viewportVJust(SEXP vp) {
    return INTEGER(getListElement(vp, "valid.just"))[1];
}

double viewportAngle(SEXP vp) {
    return numeric(getListElement(vp, "angle"), 0);
}

SEXP viewportLayout(SEXP vp) {
    return getListElement(vp, "layout");
}

SEXP viewportLayoutPosRow(SEXP vp) {
    return getListElement(vp, "valid.pos.row");
}

SEXP viewportLayoutPosCol(SEXP vp) {
    return getListElement(vp, "valid.pos.col");
}

SEXP viewportParent(SEXP vp) {
    return getListElement(vp, "parent");
}

SEXP viewportCurrentTransform(SEXP vp) {
    return getListElement(vp, "cur.trans");
}

SEXP viewportCurrentLayoutWidths(SEXP vp) {
    return getListElement(vp, "cur.widths");
}

SEXP viewportCurrentLayoutHeights(SEXP vp) {
    return getListElement(vp, "cur.heights");
}

SEXP viewportCurrentWidthCM(SEXP vp) {
    return getListElement(vp, "cur.width.cm");
}

SEXP viewportCurrentHeightCM(SEXP vp) {
    return getListElement(vp, "cur.height.cm");
}

SEXP viewportCurrentRotation(SEXP vp) {
    return getListElement(vp, "cur.rotation");
}

void fillViewportLocationFromViewport(SEXP vp, LViewportLocation *vpl) 
{
    vpl->x = viewportX(vp);
    vpl->y = viewportY(vp);
    vpl->width = viewportWidth(vp);
    vpl->height = viewportHeight(vp);
    vpl->hjust = viewportHJust(vp);
    vpl->vjust = viewportVJust(vp);
}

void fillViewportContextFromViewport(SEXP vp, 
				     LViewportContext *vpc)
{
    vpc->fontsize = viewportFontSize(vp);
    vpc->lineheight = viewportLineHeight(vp);
    vpc->xscalemin = viewportXScaleMin(vp);
    vpc->xscalemax = viewportXScaleMax(vp);
    vpc->yscalemin = viewportYScaleMin(vp);
    vpc->yscalemax = viewportYScaleMax(vp);
    vpc->origin = viewportOrigin(vp);
    vpc->hjust = viewportHJust(vp);
    vpc->vjust = viewportVJust(vp);
}

void copyViewportContext(LViewportContext vpc1, LViewportContext *vpc2)
{
    vpc2->fontsize = vpc1.fontsize;
    vpc2->lineheight = vpc1.lineheight;
    vpc2->xscalemin = vpc1.xscalemin;
    vpc2->xscalemax = vpc1.xscalemax;
    vpc2->yscalemin = vpc1.yscalemin;
    vpc2->yscalemax = vpc1.yscalemax;
    vpc2->origin = vpc1.origin;
    vpc2->hjust = vpc1.hjust;
    vpc2->vjust = vpc1.vjust;
}

/* The idea is to produce a transformation for this viewport which
 * will take any location in INCHES and turn it into a location on the 
 * Device in INCHES.
 * The reason for working in INCHES is because we want to be able to
 * do rotations as part of the transformation.
 * If "incremental" is true, then we just work from the "current"
 * values of the parent.  Otherwise, we have to recurse and recalculate
 * everything from scratch.
 */
void calcViewportTransform(SEXP vp, SEXP parent, Rboolean incremental,
			   DevDesc *dd)
{
    int i, j;
    double vpWidthCM, vpHeightCM, rotationAngle;
    double parentWidthCM, parentHeightCM;
    double xINCHES, yINCHES;
    double xadj, yadj;
    double parentAngle;
    LViewportLocation vpl;
    LViewportContext vpc, parentContext;
    LTransform thisLocation, thisRotation, thisJustification, thisTransform;
    LTransform tempTransform, parentTransform, transform;
    SEXP currentWidthCM, currentHeightCM, currentRotation;
    SEXP currentTransform;
    /* This should never be true when we are doing an incremental
     * calculation
     */
    if (isNull(parent)) {
	/* We have a top-level viewport; the parent is the device
	 */
	getDeviceSize(dd, &parentWidthCM, &parentHeightCM);
	/* For a device the transform is the identity transform
	 */
	identity(parentTransform);
	/* For a device, xmin=0, ymin=0, xmax=1, ymax=1, and
	 * origin=L_BOTTOMLEFT
	 */
	parentContext.xscalemin = 0;
	parentContext.yscalemin = 0;
	parentContext.xscalemax = 1;
	parentContext.yscalemax = 1;
	/* FIXME:  check this is true for both postscript and X11
	 */
	parentContext.origin = L_BOTTOMLEFT;
	/* FIXME:  How do I figure out the device fontsize ?
	 * From ps.options etc, ... ?
	 */
	parentContext.fontsize = 10;
	/* FIXME:  How do I figure out the device lineheight ??
	 */
	parentContext.lineheight = 1.2;
	/* The device is not rotated
	 */
	parentAngle = 0;
	fillViewportLocationFromViewport(vp, &vpl);
    } else {
	/* Get parent transform (etc ...)
	 * If necessary, recalculate the parent transform (etc ...)
	 */
	if (!incremental)
	    calcViewportTransform(parent, viewportParent(parent), 0, dd);
	/* Get information required to transform viewport location
	 */
	parentWidthCM = REAL(viewportCurrentWidthCM(parent))[0];
	parentHeightCM = REAL(viewportCurrentHeightCM(parent))[0];
	parentAngle = REAL(viewportCurrentRotation(parent))[0];
	for (i=0; i<3; i++)
	    for (j=0; j<3; j++)
		parentTransform[i][j] = 
		    REAL(viewportCurrentTransform(parent))[i +3*j];
	fillViewportContextFromViewport(parent, &parentContext);
	/* In order for the vp to get its vpl from a layout
	 * it must have specified a layout.pos and the parent
	 * must have a layout
	 * FIXME:  Actually, in addition, layout.pos.row and
	 * layout.pos.col must be valid for the layout
	 */
	if ((isNull(viewportLayoutPosRow(vp)) && 
	     isNull(viewportLayoutPosCol(vp))) ||
	    isNull(viewportLayout(parent)))
	    fillViewportLocationFromViewport(vp, &vpl);
	else
	    calcViewportLocationFromLayout(viewportLayoutPosRow(vp),
					   viewportLayoutPosCol(vp),
					   parent,
					   &vpl);
    }
    /* NOTE that we are not doing a transformLocn here because
     * we just want locations and dimensions (in INCHES) relative to 
     * the parent, NOT relative to the device.
     */
    /* First, convert the location of the viewport into CM
     */
    xINCHES = transformXtoINCHES(vpl.x, 0, parentContext,
				 viewportFontSize(vp),
				 viewportLineHeight(vp),
				 parentWidthCM, parentHeightCM, 
				 dd);
    yINCHES = transformYtoINCHES(vpl.y, 0, parentContext,
				 viewportFontSize(vp),
				 viewportLineHeight(vp),
				 parentWidthCM, parentHeightCM, 
				 dd);
    /* Calculate the width and height of the viewport in CM too
     * so that any viewports within this one can do transformations
     */
    vpWidthCM = transformWidthtoINCHES(vpl.width, 0, parentContext,
				       viewportFontSize(vp),
				       viewportLineHeight(vp),
				       parentWidthCM, parentHeightCM,
				       dd)*2.54;
    vpHeightCM = transformHeighttoINCHES(vpl.height, 0, parentContext,
					 viewportFontSize(vp),
					 viewportLineHeight(vp),
					 parentWidthCM, 
					 parentHeightCM, 
					 dd)*2.54;
    /* Determine justification required
     */
    justification(vpWidthCM, vpHeightCM, vpl.hjust, vpl.vjust,
		  &xadj, &yadj);
    /* Next, produce the transformation to add the location of
     * the viewport to the location.
     */
    /* Produce transform for this viewport
     */
    translation(xINCHES, yINCHES, thisLocation);
    if (viewportAngle(vp) != 0)
	rotation(viewportAngle(vp), thisRotation);
    else
	identity(thisRotation);
    translation(xadj/2.54, yadj/2.54, thisJustification);
    /* Position relative to origin of rotation THEN rotate.
     */
    multiply(thisJustification, thisRotation, tempTransform);
    /* Translate to bottom-left corner.
     */
    multiply(tempTransform, thisLocation, thisTransform);
    /* Combine with parent's transform
     */
    multiply(thisTransform, parentTransform, transform);
    /* Sum up the rotation angles
     */
    rotationAngle = parentAngle + viewportAngle(vp);
    /* Finally, allocate the rows and columns for this viewport's
     * layout if it has one
     */
    if (!isNull(viewportLayout(vp))) {
	fillViewportContextFromViewport(vp, &vpc);
	calcViewportLayout(vp, vpWidthCM, vpHeightCM, vpc, dd);
    }
    /* Record all of the answers in the viewport
     * (the layout calculations are done within calcViewportLayout)
     */
    PROTECT(currentWidthCM = allocVector(REALSXP, 1));
    REAL(currentWidthCM)[0] = vpWidthCM;
    PROTECT(currentHeightCM = allocVector(REALSXP, 1));
    REAL(currentHeightCM)[0] = vpHeightCM;
    PROTECT(currentRotation = allocVector(REALSXP, 1));
    REAL(currentRotation)[0] = rotationAngle;
    PROTECT(currentTransform = allocMatrix(REALSXP, 3, 3));
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    REAL(currentTransform)[i + 3*j] = transform[i][j];
    setListElement(vp, "cur.width.cm", currentWidthCM);
    setListElement(vp, "cur.height.cm", currentHeightCM);
    setListElement(vp, "cur.rotation", currentRotation);
    setListElement(vp, "cur.trans", currentTransform);
    UNPROTECT(4);
}


