# EFDVS-144
from cleanSVG import CleanSVG
import os
import sys
 
input_file = os.path.join(sys.argv[1])
output_file = sys.argv[1]
svg = CleanSVG(input_file)
svg.setDecimalPlaces(1)
svg.extractStyles()
svg.applyTransforms()
svg.write(output_file)
