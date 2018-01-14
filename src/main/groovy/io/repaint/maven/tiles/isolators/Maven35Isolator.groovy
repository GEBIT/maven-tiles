package io.repaint.maven.tiles.isolators

import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode

import java.io.File

import org.apache.maven.MavenExecutionException
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.building.ModelProblem
import org.apache.maven.model.building.ModelProblemCollector
import org.apache.maven.model.merge.MavenModelMerger
import org.apache.maven.project.MavenProject
import org.eclipse.aether.repository.RepositoryPolicy

/**
 *
 * @author: Richard Vowles - https://plus.google.com/+RichardVowles
 * @author: Mark Derricutt - https://plus.google.com/+MarkDerricutt
 */
class Maven35Isolator extends AetherIsolator {
	def projectArtifactsCache

	Maven35Isolator(MavenSession mavenSession) throws MavenExecutionException {
		super(mavenSession)
	}

	protected void setupIsolateClasses(MavenSession mavenSession) {
		// lets fail fast
		super.setupIsolateClasses(mavenSession)
		Class projectArtifactsCacheClass = Class.forName("org.apache.maven.plugin.ProjectArtifactsCache")
		projectArtifactsCache = mavenSession.container.lookup(projectArtifactsCacheClass)
	}

	void flushProjectArtifacts(MavenProject project) {
		projectArtifactsCache.cache.iterator().with { iterator ->
			iterator.each { entry ->
				def cacheKey = entry.key
				if (cacheKey.groupId == project.groupId
						&& cacheKey.artifactId == project.artifactId
						&& cacheKey.version == project.version) {
					iterator.remove()
				}
			}
		}
	}
}
