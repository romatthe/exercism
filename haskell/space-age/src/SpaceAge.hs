module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth   age = onEarth age
ageOn Mercury age = onEarth age / 0.2408467 
ageOn Venus   age = onEarth age / 0.61519726
ageOn Mars    age = onEarth age / 1.8808158
ageOn Jupiter age = onEarth age / 11.862615
ageOn Saturn  age = onEarth age / 29.447498 
ageOn Uranus  age = onEarth age / 84.016846
ageOn Neptune age = onEarth age / 164.79132

onEarth :: Float -> Float
onEarth t = t / (3600 * 24) / 365.25
