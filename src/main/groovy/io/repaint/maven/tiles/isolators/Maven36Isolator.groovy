package io.repaint.maven.tiles.isolators

import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode

import java.io.File
import java.util.function.BiPredicate

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
class Maven36Isolator extends AetherIsolator {
	def projectArtifactsCache

	Maven36Isolator(MavenSession mavenSession) throws MavenExecutionException {
		super(mavenSession)
	}

	protected void setupIsolateClasses(MavenSession mavenSession) {
		// lets fail fast
		super.setupIsolateClasses(mavenSession)
		Class projectArtifactsCacheClass = Class.forName("org.apache.maven.project.artifact.ProjectArtifactsCache")
		projectArtifactsCache = mavenSession.container.lookup(projectArtifactsCacheClass)
	}

	// m2e: flush cached artifacts in case a *tile* adds further dependencies. Does not happen in plain Maven,
	// nor in mvnd.
	void flushProjectArtifacts(MavenProject project) {
		if (projectArtifactsCache.cache == null) {
			return
		}
		// Support for mvnd's TimestampCache
		if (projectArtifactsCache.cache.getMetaClass().respondsTo(
				projectArtifactsCache.cache,
				'removeIf',
				BiPredicate)) {
				// System.err.println("mmmmmmmmmmmmmmmmmmmmmmmmm " + projectArtifactsCache.cache.map.size())
				// Note: we don't catch any exceptions here so that we immediately notice when it breaks.
				projectArtifactsCache.cache.removeIf { k, v ->
				throw new RuntimeException(k)
					k.groupId == project.groupId &&
					k.artifactId == project.artifactId &&
					k.version == project.version
				}
		} else {
			// Maven's DefaultProjectArtfifactsCache
			// System.err.println("bbbbbbbbbbbbbbbbbbbbbbbb " + projectArtifactsCache.cache.size())
			projectArtifactsCache.cache.iterator().with { iterator ->
				iterator.each { entry ->
					if (entry.hasProperty('key')) {
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
	}
}
