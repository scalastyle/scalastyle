
usage() {
	echo "usage: $0 [deploy]" 1>&2
	exit 2
}

case $1 in
deploy)	phase=deploy ;;
*)	phase=package ;;
esac

replace_version() {
	element=$1
	old=$2
	new=$3

	perl -p -i.bak -e "s;<$element>$old</$element>;<$element>$new</$element>;" pom.xml
}

replace_versions() {
	version=$1

	case $version in
	2.9.2) scalatest_version=2.0.M4 ;;
	2.9.3) scalatest_version=2.0.M5b ;;
	esac

	replace_version artifactId scalastyle_2.10 scalastyle_$version
	replace_version scala.version 2.10.0 $version
	replace_version scala.library.version 2.10.0 $version
	replace_version scalariform.artifactId scalariform_2.10 scalariform_$version

	replace_version scalatest.artifactId scalatest_2.10 scalatest_$version

	replace_version scalatest.version 2.0.M6-SNAP9 $scalatest_version

	replace_version scopt.artifactId scopt_2.10 scopt_2.9.2

	rm pom.xml.bak
}

dir=`pwd`

for version in 2.9.2 2.9.3
do
	builddir="build-$version"

	rm -rf target/$builddir
	mkdir -p target
	( cd target; git clone --depth=1 file://$dir $builddir; cd $builddir; rm -rf .git; replace_versions $version; mvn $phase)
done
