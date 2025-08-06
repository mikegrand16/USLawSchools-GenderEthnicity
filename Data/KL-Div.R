kl.div <- function(p1, p2, q1, q2, c, epsilon = 1e-10){
  p_smooth <- (p1 + epsilon)/(p1+p2+2*epsilon)
  q_smooth <- (q1 + epsilon)/(q1+q2+2*epsilon)
  print(p_smooth)
  print(q_smooth)
  print(p1+p2+2*epsilon)
  print(q1+q2+2*epsilon)
  print(c*(q1 + epsilon))
  print(p_smooth*log((c*(p1 + epsilon))/(c*(q1 + epsilon))))
  print(q_smooth*log((c*(q1 + epsilon))/(c*(p1 + epsilon))))
  KL_PQ <- p_smooth*log((c*(p1 + epsilon))/(c*(q1 + epsilon))) + p_smooth*log((c*(p2 + epsilon))/(c*(q2 + epsilon)))
  KL_QP <- q_smooth*log((c*(q1 + epsilon))/(c*(p1 + epsilon))) + q_smooth*log((c*(q2 + epsilon))/(c*(p2 + epsilon)))
  print(KL_PQ)
  print(KL_QP)
  return(KL_PQ + KL_QP)
}

kl.div(0.2,0.8, 0, 1, c = 1, epsilon = 1e-10)
kl.div(20,80,0,100, c=1, epsilon = 1e-10)
