scala_library(name='scala',
  provides = scala_artifact(
    org = 'com.twitter',
    name = 'finagle-toggle',
    repo = artifactory,
  ),
  dependencies=[
    '3rdparty/jvm/com/fasterxml/jackson/core:jackson-core',
    '3rdparty/jvm/com/fasterxml/jackson/core:jackson-databind',
    '3rdparty/jvm/com/fasterxml/jackson/module:jackson-module-scala',
    'util/util-app/src/main/java',
    'util/util-app/src/main/scala',
    'util/util-core',
  ],
  fatal_warnings=True,
  strict_deps=True,
  sources=rglobs('*.scala'),
)
