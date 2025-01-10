//VERSION=3
//Other S2L2A specific data (Aerosol Optical Thickness, Scene Classification, Snow and Cloud probabilities, Sun and View angles)

function setup() {
  return {
    input: [{bands:["sunAzimuthAngles", "sunZenithAngles", "viewAzimuthMean", "viewZenithMean"]}],
    output: {
      id: "default",
      bands: 8,
      sampleType: SampleType.FLOAT32
    }
  }
}

function evaluatePixel(sample) {
    return [ sample.sunAzimuthAngles, sample.sunZenithAngles, sample.viewAzimuthMean, sample.viewZenithMean]
}
