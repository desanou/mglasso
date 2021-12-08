import numpy as np
import parsimony.estimators as estimators
import parsimony.algorithms as algorithms
import parsimony.functions.nesterov.tv as tv
import sklearn.preprocessing 
from scipy import sparse


def Ak_from_pairs(k,p):
    Ak = sparse.lil_matrix((int(p*(p-1)/2),p*p))
    ij=0
    for i in range(0,p-1):
        for j in range(i+1,p):
            #print(i*p+k,j*p+k,ij)
            if (i==k)|(j==k):
                Ak[ij,i*p+j]=1
                Ak[ij,j*p+i]=-1
            else:
                Ak[ij,i*p+k]=1
                Ak[ij,j*p+k]=-1
            ij=ij+1
    #return(Ak)
       
    to_keep = list(set(range(Ak.shape[1]))-set(range(0,p*p,p+1)))    
    Aknew = sparse.lil_matrix(sparse.csr_matrix(Ak)[:,to_keep])     
    return(Aknew)




def linear_operator_from_num_variables(num_variables):
    """Generates the linear operator for the TV lasso Nesterov function
    from number of variables.

    Parameters:
    ----------
    num_variables : Integer. The total number of variables, including the
            intercept variable(s).

    """
    A = list()
    for k in range(0,num_variables):
        Ak = Ak_from_pairs(k,num_variables)
        A.append(Ak.tocsr())
    return A

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


def conesta_rwrapper(X, lam1, lam2):
  X=np.array(X)
  n=X.shape[0]
  p=X.shape[1]
  
  X=sklearn.preprocessing.scale(X)
  y=X.reshape(n*p,1,order='F')
  Xvec=np.delete(np.kron(np.identity(p),X),range(0,p*p,p+1),axis=1)
  A=linear_operator_from_num_variables(p)
  
  l = lam1  # l1 lasso coefficient
  k = 0.0  # l2 ridge regression coefficient
  g = lam2 
  
  hgmm = estimators.LinearRegressionL1L2TV(l, k, g, A, mu=0.001,
                                           algorithm=algorithms.proximal.CONESTA(max_iter=1000), mean=False)
  res = hgmm.fit(Xvec,y)
  Beta=beta2Beta(res.beta,p)
  #print(res.score(Xvec, y))
  
  return(Beta)
