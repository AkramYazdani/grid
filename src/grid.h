#include <Rinternals.h>
#include <Rgraphics.h>  
#include <Rmath.h>

/* All lattice type names are prefixed with an "L" 
 * All lattice global variable names are prefixe with an "L_" 
 */

typedef double LTransform[3][3];

typedef double LLocation[3];

typedef enum {
    L_adding = 0,
    L_subtracting = 1,
    L_summing = 2,
    L_plain = 3,
    L_maximising = 4,
    L_minimising = 5,
    L_multiplying = 6
} LNullArithmeticMode;

/* NOTE: The order of the enums here must match the order of the
 * strings in viewport.R
 */
typedef enum {
    L_NPC = 0,
    L_CM = 1,
    L_INCHES = 2,
    L_LINES = 3,
    L_NATIVE = 4,
    L_NULL = 5, /* only used in layout specifications (?) */
    L_SNPC = 6,
    L_MM = 7,
    /* Some units based on TeX's definition thereof
     */
    L_POINTS = 8,           /* 72.27 pt = 1 in */
    L_PICAS = 9,            /* 1 pc     = 12 pt */
    L_BIGPOINTS = 10,       /* 72 bp    = 1 in */
    L_DIDA = 11,            /* 1157 dd  = 1238 pt */
    L_CICERO = 12,          /* 1 cc     = 12 dd */
    L_SCALEDPOINTS = 13,    /* 65536 sp = 1pt */
    /* Some units which require an object to query for a value.
     */
    L_STRINGWIDTH = 14,
    L_STRINGHEIGHT = 15,
    /* L_LINES now means multiples of the line height.
     * This is multiples of the font size.
     */
    L_CHAR = 18,
    L_GROBWIDTH = 19,
    L_GROBHEIGHT = 20,
    L_MYLINES = 21,
    L_MYCHAR = 22
} LUnit;

typedef enum {
    L_LEFT = 0,
    L_RIGHT = 1,
    L_BOTTOM = 2,
    L_TOP = 3,
    L_CENTRE = 4,
    L_CENTER = 5
} LJustification;

typedef enum {
    L_BOTTOMLEFT = 0,
    L_TOPLEFT = 1,
    L_BOTTOMRIGHT = 2,
    L_TOPRIGHT = 3
} LOrigin;

/* An arbitrarily-oriented rectangle.
 * The vertices are assumed to be in order going anitclockwise 
 * around the rectangle.
 */
typedef struct {
    double x1;
    double x2;
    double x3;
    double x4;
    double y1;
    double y2;
    double y3;
    double y4;
} LRect;

/* A description of the location of a viewport */
typedef struct {
    SEXP x;
    SEXP y;
    SEXP width;
    SEXP height;
    LJustification hjust;
    LJustification vjust;
} LViewportLocation;

/* Components of a viewport which provide coordinate information
 * for children of the viewport
 */
typedef struct {
    double fontsize;
    double lineheight;
    double xscalemin;
    double xscalemax;
    double yscalemin;
    double yscalemax;
    int origin;
    int hjust;
    int vjust;
} LViewportContext;

/* From matrix.c */
double locationX(LLocation l);

double locationY(LLocation l);

void copyTransform(LTransform t1, LTransform t2);

void identity(LTransform m);

void translation(double tx, double ty, LTransform m);

void scaling(double sx, double sy, LTransform m);

void rotation(double theta, LTransform m);

void multiply(LTransform m1, LTransform m2, LTransform m);

void location(double x, double y, LLocation v);

void trans(LLocation vin, LTransform m, LLocation vout);

/* From unit.c */
SEXP unit(double value, int unit);

double unitValue(SEXP unit, int index);

int unitUnit(SEXP unit, int index);

SEXP unitData(SEXP unit, int index);

int unitLength(SEXP u);

int L_nullLayoutMode;

int pureNullUnit(SEXP unit, int index);

double transformX(SEXP x, int index, LViewportContext vpc,
		  double fontsize, double lineheight,
		  double widthCM, double heightCM,
		  DevDesc *dd);

double transformY(SEXP y, int index, LViewportContext vpc,
		  double fontsize, double lineheight,
		  double widthCM, double heightCM,
		  DevDesc *dd);

double transformWidth(SEXP width, int index, LViewportContext vpc,
		      double fontsize, double lineheight,
		      double widthCM, double heightCM,
		      DevDesc *dd);

double transformHeight(SEXP height, int index, LViewportContext vpc,
		       double fontsize, double lineheight,
		       double widthCM, double heightCM,
		       DevDesc *dd);

double transformXtoNative(SEXP x, int index,
			  LViewportContext vpc,
			  double fontsize, double lineheight,
			  double widthCM, double heightCM,
			  DevDesc *dd);

double transformYtoNative(SEXP y, int index,
			  LViewportContext vpc,
			  double fontsize, double lineheight,
			  double widthCM, double heightCM,
			  DevDesc *dd);

double transformWidthtoNative(SEXP width, int index,
			      LViewportContext vpc,
			      double fontsize, double lineheight,
			      double widthCM, double heightCM,
			      DevDesc *dd);

double transformHeighttoNative(SEXP height, int index,
			       LViewportContext vpc,
			       double fontsize, double lineheight,
			       double widthCM, double heightCM,
			       DevDesc *dd);

double transformXtoINCHES(SEXP x, int index, LViewportContext vpc,
			  double fontsize, double lineheight,
			  double widthCM, double heightCM,
			  DevDesc *dd);

double transformYtoINCHES(SEXP y, int index, LViewportContext vpc,
			  double fontsize, double lineheight,
			  double widthCM, double heightCM,
			  DevDesc *dd);

void transformLocn(SEXP x, SEXP y, int index, LViewportContext vpc,
		   double fontsize, double lineheight,
		   double widthCM, double heightCM,
		   DevDesc *dd,
		   LTransform t,
		   double *xx, double *yy);

double transformWidthtoINCHES(SEXP w, int index, LViewportContext vpc,
			      double fontsize, double lineheight,
			      double widthCM, double heightCM,
			      DevDesc *dd);

double transformHeighttoINCHES(SEXP h, int index, LViewportContext vpc,
			       double fontsize, double lineheight,
			       double widthCM, double heightCM,
			       DevDesc *dd);

void transformDimn(SEXP w, SEXP h, int index, LViewportContext vpc,
		   double fontsize, double lineheight,
		   double widthCM, double heightCM,
		   DevDesc *dd,
		   double rotationAngle,
		   double *ww, double *hh);

/* From just.c */
double justifyX(double x, double width, int hjust);

double justifyY(double y, double height, int vjust);

double convertJust(int vjust);

void justification(double width, double height, int hjust, int vjust,
		   double *hadj, double *vadj);

/* From util.c */
SEXP getListElement(SEXP list, char *str);

double numeric(SEXP x, int index);

void rect(double x1, double x2, double x3, double x4, 
	  double y1, double y2, double y3, double y4, 
	  LRect *r);

void copyRect(LRect r1, LRect *r);

int intersect(LRect r1, LRect r2);

void textRect(double x, double y, char *string, double xadj, double yadj,
	      double rot, DevDesc *dd, LRect *r);

/* From viewport.c */
SEXP viewportX(SEXP vp);

SEXP viewportY(SEXP vp);

SEXP viewportWidth(SEXP vp);

SEXP viewportHeight(SEXP vp);

double viewportFontSize(SEXP vp);

double viewportLineHeight(SEXP vp);

double viewportXScaleMin(SEXP vp);

double viewportXScaleMax(SEXP vp);

double viewportYScaleMin(SEXP vp);

double viewportYScaleMax(SEXP vp);

int viewportOrigin(SEXP vp);

int viewportHJust(SEXP v);

int viewportVJust(SEXP vp);

SEXP viewportLayout(SEXP vp);

SEXP viewportParent(SEXP vp);

SEXP viewportCurrentTransform(SEXP vp);

SEXP viewportCurrentLayoutWidths(SEXP vp);

SEXP viewportCurrentLayoutHeights(SEXP vp);

SEXP viewportCurrentWidthCM(SEXP vp);

SEXP viewportCurrentHeightCM(SEXP vp);

SEXP viewportCurrentRotation(SEXP vp);

void fillViewportContextFromViewport(SEXP vp, LViewportContext *vpc);

void copyViewportContext(LViewportContext vpc1, LViewportContext *vpc2);

void viewportTransform(SEXP vp, SEXP parent, DevDesc *dd,
		       double *vpWidthCM, double *vpHeightCM,
		       LTransform transform, double *rotationAngle);

/* From layout.c */
void calcViewportLayout(SEXP viewport,
			double parentWidthCM,
			double parentHeightCM,
			LViewportContext parentContext,
			DevDesc *dd);
void calcViewportLocationFromLayout(SEXP layoutPosRow,
				    SEXP layoutPosCol,
				    SEXP parent,
				    LViewportLocation *vpl);

/* From grid.c */
LTransform L_viewportTransform;
double L_rotationAngle;
double L_viewportWidthCM;
double L_viewportHeightCM;
LViewportContext L_viewportContext;

void getDeviceSize(DevDesc *dd, double *devWidthCM, double *devHeightCM);

