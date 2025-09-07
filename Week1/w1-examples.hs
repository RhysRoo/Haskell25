import Prelude (pi, (*), Float)

-- worked examples
circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = circumferenceOfCircle d * h