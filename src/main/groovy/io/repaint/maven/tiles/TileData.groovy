package io.repaint.maven.tiles
import groovy.transform.CompileStatic
import io.repaint.maven.tiles.TilesMavenLifecycleParticipant.ArtifactModel

import org.apache.maven.artifact.Artifact

/**
 * Holds the current tile data in the current MavenSession. Prevents race condition in access when using in m2e.
 *
 * @author: ErwinTratar
 */
@CompileStatic
class TileData {
	Map<String, ArtifactModel> processedTiles = [:].asSynchronized()
	List<String> tileDiscoveryOrder = [].asSynchronized()
	Map<String, Artifact> unprocessedTiles = [:].asSynchronized()
	List<String> ignoredTiles = [].asSynchronized()
}
