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
 */
void viewportTransform(SEXP vp, SEXP parent, DevDesc *dd,
		       double *vpWidthCM, double *vpHeightCM,
		       LTransform transform, double *rotationAngle)
{
    double parentWidthCM, parentHeightCM;
    double xINCHES, yINCHES;
    double xadj, yadj;
    double parentAngle;
    LViewportLocation vpl;
    LViewportContext parentContext;
    LTransform thisLocation, thisRotation, thisJustification, thisTransform;
    LTransform tempTransform, parentTransform;
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
	/* Get parent transform (and widthCM and heightCM)
	 */
	viewportTransform(parent, viewportParent(parent), dd,
			  &parentWidthCM, &parentHeightCM,
			  parentTransform, &parentAngle);
	/* Get information required to transform viewport location
	 */
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
					   viewportLayout(parent),
					   parentWidthCM,
					   parentHeightCM,
					   parentContext,
					   dd,
					   &vpl);
    }
    /* First, convert the location of the viewport into CM
     */
    xINCHES = transformXtoINCHES(vpl.x, 0, parentContext,
				 parentWidthCM, parentHeightCM, 
				 dd);
    yINCHES = transformYtoINCHES(vpl.y, 0, parentContext,
				 parentWidthCM, parentHeightCM, 
				 dd);
    /* Calculate the width and height of the viewport in CM too
     * so that any viewports within this one can do transformations
     */
    *vpWidthCM = transformWidthtoINCHES(vpl.width, 0, parentContext,
					parentWidthCM, parentHeightCM,
					dd)*2.54;
    *vpHeightCM = transformHeighttoINCHES(vpl.height, 0, parentContext,
					  parentWidthCM, 
					  parentHeightCM, 
					  dd)*2.54;
    /* Determine justification required
     */
    justification(*vpWidthCM, *vpHeightCM, vpl.hjust, vpl.vjust,
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
    *rotationAngle = parentAngle + viewportAngle(vp);
}


