import Control.Monad.Writer

tailor_sin (a,x,i,fact,sint,z) = tell [sint] >> if (xx/factt > 0.00001) then tailor_sin (a,xx,ii,factt,sintt,zz) else return sint
	where 
		xx = x*a*a
		ii = i+2
		factt = fact * (ii-1) * ii
		sintt=sint+z*x/fact;
		zz = z*(-1)


sin_with_jurnal x = runWriter $ tailor_sin (x,x,1,1,0,1)

cos_with_jurnal x = runWriter $ tailor_sin (x,x*x,2,2,1,-1)
