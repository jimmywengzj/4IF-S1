/*
 * This is sample code generated by rpcgen.
 * These are only templates and you can use them
 * as a guideline for developing your own functions.
 */

#include "geometrie.h"

int *
surface_rectangle_1_svc(rectangle *rect, struct svc_req *rqstp)
{
	static int  result;

	/*
	 * insert server code here
	 */

  /* ce que je rentre à la main */
result = (rect -> p1.x - rect -> p2.x) *
          (rect -> p1.y - rect -> p2.y);
 /*fin de ce que je rentre à la main */

	return &result;
}

rectangle *
creer_rectangle_1_svc(coordonnees *coord, struct svc_req *rqstp)
{
	static rectangle  result;

	/*
	 * insert server code here
	 */

  /* ce que je rentre à la main */
result.p1.x = coord -> x1; result.p1.y = coord -> y1;
result.p2.x = coord -> x2; result.p2.y = coord -> y2; 
  /*fin de ce que je rentre à la main */


	return &result;
}

booleen *
inclus_1_svc(param_inclus *param, struct svc_req *rqstp)
{
	static booleen  result;

	/*
	 * insert server code here
	 */

  /* ce que je rentre à la main */
result = (param -> p.x >= param -> rect.p1.x) &&
         (param -> p.x <= param -> rect.p2.x) &&
         (param -> p.y >= param -> rect.p1.y) &&
         (param -> p.y <= param -> rect.p2.y);
  /*fin de ce que je rentre à la main */


	return &result;
}
