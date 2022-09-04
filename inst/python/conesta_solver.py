import parsimony.estimators as estimators
import parsimony.algorithms as algorithms
import sklearn.preprocessing 
import numpy as np
import scipy
from scipy import sparse

def conesta_py(X, lam1, lam2, beta_warm=None, type_="initial", W_=None, mean_ = False, max_iter_=1e4, prec_=1e-2):
  X=np.array(X)
  n=X.shape[0]
  p=X.shape[1]
  
  X=sklearn.preprocessing.scale(X)
  y=X.reshape(n*p,1,order='F')
  Xvec=np.delete(np.kron(np.identity(p),X),range(0,p*p,p+1),axis=1)
  A_=linear_operator_from_num_variables(p, type_, W_)
  
  if beta_warm is not None:
    beta_warm = np.array(beta_warm)
    hgmm = estimators.LinearRegressionL1L2TV(l1 = lam1, l2 = 0.0, tv = lam2, A = A_, start_vector=beta_warm,
                                           algorithm=algorithms.proximal.CONESTA(max_iter=max_iter_, eps=prec_), mean=mean_)
  if beta_warm is None:
    hgmm = estimators.LinearRegressionL1L2TV(l1 = lam1, l2 = 0.0, tv = lam2, A = A_,
                                           algorithm=algorithms.proximal.CONESTA(max_iter=max_iter_, eps=prec_), mean=mean_)
  res = hgmm.fit(Xvec,y)
  Beta=beta2Beta(res.beta,p)

  return(Beta)

def Ak_from_pairs(k,p,type_,W):
    Ak = sparse.lil_matrix((int(p*(p-1)/2),p*p))
    ij=0
    if W is None:
      W = np.ones((p,p))
    if type_=="initial" or type_=="pcor" or type_=="adapt":
        for i in range(0,p-1):
            for j in range(i+1,p):
                if (i==k)|(j==k):
                    Ak[ij,i*p+j]=1*W[i,j]
                    Ak[ij,j*p+i]=-1*W[i,j]
                else:
                    Ak[ij,i*p+k]=1*W[i,j]
                    Ak[ij,j*p+k]=-1*W[i,j]
                ij=ij+1
    elif type_=="wr":# discarding beta_ij and beta_ji
        for i in range(0,p-1):
            for j in range(i+1,p):
                if (i==k)|(j==k):
                    Ak[ij,i*p+j]=0
                    Ak[ij,j*p+i]=0
                else:
                    Ak[ij,i*p+k]=1
                    Ak[ij,j*p+k]=-1
                ij=ij+1

    to_keep = list(set(range(Ak.shape[1]))-set(range(0,p*p,p+1)))
    Aknew = sparse.lil_matrix(sparse.csr_matrix(Ak)[:,to_keep])
    return(Aknew)

def linear_operator_from_num_variables(num_variables, type_, W):
    """Generates the linear operator for the TV lasso Nesterov function
    from number of variables.

    Parameters:
    ----------
    num_variables : Integer. The total number of variables, including the
            intercept variable(s).

    """
    A = list()
    for k in range(0,num_variables):
        Ak = Ak_from_pairs(k,num_variables,type_,W)
        A.append(Ak.tocsr())
    return A

#function for filling a pxp matrix from a (p-1)xp vector
def beta2Beta(beta,p): 
    Beta=np.zeros((p,p))
    for j in range(0,(p-1)):
        for i in range(0,p):
            k=i
            l=j
            if j>=i:
                l=j+1
            Beta[k,l]=beta[i*(p-1)+j]
    return(Beta)  


def precision2regression(K):
    p=K.shape[0]
    M=np.zeros((p,p))
    for i in range(0,p):
        for j in range(0,p):
            if i!=j:
                M[i,j]= - K[i,j]/K[i,i]
    return(M) 
