package io.repaint.maven.tiles.isolators

import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode
import org.apache.maven.MavenExecutionException
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Model
import org.apache.maven.model.Scm
import org.apache.maven.model.building.FileModelSource
import org.apache.maven.model.building.ModelProblemCollector
import org.apache.maven.model.inheritance.DefaultInheritanceAssembler
import org.apache.maven.model.merge.MavenModelMerger

/**
 *
 * @author: Richard Vowles - https://plus.google.com/+RichardVowles
 * @author: Mark Derricutt - https://plus.google.com/+MarkDerricutt
 */
class AetherIsolator extends BaseMavenIsolator {

	@Override
	ModelProblemCollector createModelProblemCollector() {
		def collected = []

		return [
			problems: collected,
		  add: { req ->
			  collected.add(req)
		  }
		] as ModelProblemCollector
	}

	@Override
	@CompileStatic(TypeCheckingMode.SKIP)
	def createModelData(Model model, File pomFile) {
		return org.apache.maven.model.building.ModelData.newInstance(pomFile ? new FileModelSource(pomFile) : null, model, model.groupId, model.artifactId, model.version)
	}

	AetherIsolator(MavenSession mavenSession) throws MavenExecutionException {
		super(mavenSession)
	}

	/**
	 * Yes, these mutate state but I need to fail fast if this isn't the right match.
	 *
	 * @param mavenSession - we need to ask Plexus for the range resolver.
	 */
	protected void setupIsolateClasses(MavenSession mavenSession) {
		// lets fail fast
		Class versionRangeResolverClass = Class.forName("org.eclipse.aether.impl.VersionRangeResolver")
		versionRangeResultClass = Class.forName("org.eclipse.aether.resolution.VersionRangeResult")
		versionRangeRequestClass = Class.forName("org.eclipse.aether.resolution.VersionRangeRequest")

		versionRangeResolver = mavenSession.container.lookup(versionRangeResolverClass)
	}
}
