/***********************************************************************************************************************
 *
 * Maven Tiles
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************/
package io.repaint.maven.tiles

import static groovy.test.GroovyAssert.shouldFail
import static org.mockito.Mockito.mock

import org.apache.maven.MavenExecutionException
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.repository.ArtifactRepository
import org.apache.maven.artifact.resolver.ArtifactNotFoundException
import org.apache.maven.artifact.resolver.ArtifactResolutionException
import org.apache.maven.artifact.resolver.ArtifactResolver
import org.apache.maven.execution.DefaultMavenExecutionRequest
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Build
import org.apache.maven.model.Model
import org.apache.maven.model.Parent
import org.apache.maven.model.Plugin
import org.apache.maven.model.building.ModelBuilder
import org.apache.maven.model.building.ModelBuildingRequest
import org.apache.maven.model.building.ModelData
import org.apache.maven.model.building.ModelProblemCollector
import org.apache.maven.model.io.xpp3.MavenXpp3Reader
import org.apache.maven.project.MavenProject
import org.codehaus.plexus.logging.Logger
import org.codehaus.plexus.util.xml.Xpp3DomBuilder
import org.eclipse.aether.DefaultRepositoryCache
import org.eclipse.aether.DefaultRepositorySystemSession
import org.eclipse.aether.RepositorySystemSession
import org.junit.AfterClass
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.runners.MockitoJUnitRunner

import groovy.transform.CompileStatic
import io.repaint.maven.tiles.isolators.MavenVersionIsolator

/**
 * If testMergeTile fails with java.io.FileNotFoundException: src/test/resources/licenses-tiles-pom.xml
 * (No such file or directory)) when running the test from your IDE, make sure you configure the Working
 * Directory as maven-tiles/tiles-maven-plugin (absolute path)
 */
@CompileStatic
@RunWith(MockitoJUnitRunner.class)
public class TilesMavenLifecycleParticipantTest {

	TilesMavenLifecycleParticipant participant
	ArtifactResolver mockResolver
	Logger logger

	public final static String PERFORM_RELEASE = "performRelease"
	static String performRelease

	@BeforeClass
	public static void storePerformRelease() {
		performRelease = System.getProperty(PERFORM_RELEASE)
	}

	@AfterClass
	public static void resetPerformRelease() {
		if (!performRelease) {
			System.clearProperty(PERFORM_RELEASE)
		} else {
			System.setProperty(PERFORM_RELEASE, performRelease)
		}
	}

	@Before
	public void setupParticipant() {
		this.participant = new TilesMavenLifecycleParticipant()

		mockResolver = mock(ArtifactResolver.class)
		logger = [
			warn: { String msg -> println msg },
			info:{ String msg -> println msg },
			debug: { String msg -> println msg },
			isDebugEnabled: { return true }
		] as Logger

		stuffParticipant()

		System.clearProperty(PERFORM_RELEASE)
	}

	void stuffParticipant() {
		participant.logger = logger
		participant.resolver = mockResolver
		participant.mavenVersionIsolate = createFakeIsolate()
	}

	protected MavenVersionIsolator createFakeIsolate() {
		return new MavenVersionIsolator() {
					@Override
					void resolveVersionRange(Artifact tileArtifact) {
					}

					@Override
					ModelProblemCollector createModelProblemCollector() {
						return [
							add: { req ->
							}
						] as ModelProblemCollector
					}

					@Override
					ModelData createModelData(Model model, File pomFile) {
						return null
					}

					@Override
					void flushProjectArtifacts(MavenProject project) {
					}
				}
	}

	public Artifact getTileTestCoordinates() {
		return participant.getArtifactFromCoordinates("it.session.maven.tiles", "session-license-tile", "xml", "", "0.8-SNAPSHOT")
	}


	@Test
	public void ensureSnapshotFailsOnRelease() {
		Artifact snapshot = getTileTestCoordinates()
		System.setProperty(PERFORM_RELEASE, "true")
		shouldFail(MavenExecutionException) {
			participant.resolveTile(null, null, snapshot)
		}
	}

	@Test
	public void ensureBadArtifactsFail() {
		Artifact badbadbad = participant.getArtifactFromCoordinates("bad", "bad", "bad", "bad", "bad")

		participant.resolver = [
			resolve: { Artifact artifact, List<ArtifactRepository> remoteRepositories, ArtifactRepository localRepository ->
				throw new ArtifactResolutionException("failed", badbadbad)
			}
		] as ArtifactResolver

		shouldFail(MavenExecutionException) {
			participant.resolveTile(null, null, badbadbad)
		}
		participant.resolver = [
			resolve: { Artifact artifact, List<ArtifactRepository> remoteRepositories, ArtifactRepository localRepository ->
				throw new ArtifactNotFoundException("failed", badbadbad)
			}
		] as ArtifactResolver

		shouldFail(MavenExecutionException) {
			participant.resolveTile(null, null, badbadbad)
		}
	}

	@Test
	public void testGetArtifactFromCoordinates() {
		Artifact artifact = participant.getArtifactFromCoordinates("dummy", "dummy", "xml", "classy", "1")

		assert artifact != null

		artifact.with {
			assert groupId == "dummy"
			assert artifactId == "dummy"
			assert type == "xml"
			assert classifier == "classy"
			assert version == "1"
		}
	}

	@Test
	public void testGavFromString() {
		Artifact dummy = participant.turnPropertyIntoUnprocessedTile("my:long:feet", null, null)

		assert dummy.version == 'feet'
		assert dummy.artifactId == 'long'
		assert dummy.groupId == 'my'
		assert dummy.classifier == ''
		assert dummy.type == 'xml'

		Artifact dummy2 = participant.turnPropertyIntoUnprocessedTile("my:long:sore:smelly:feet", null, null)

		assert dummy2.version == 'feet'
		assert dummy2.artifactId == 'long'
		assert dummy2.groupId == 'my'
		assert dummy2.classifier == 'smelly'
		assert dummy2.type == 'sore'

		// too short
		shouldFail(MavenExecutionException) {
			participant.turnPropertyIntoUnprocessedTile("my:long", null, null)
		}

		// too long
		shouldFail(MavenExecutionException) {
			participant.turnPropertyIntoUnprocessedTile("my:long:feet:and:smelly:shoes", null, null)
		}
	}

	@Test
	public void canLoadExtendedTiles() {
		Artifact artifact = participant.turnPropertyIntoUnprocessedTile("io.repaint.tiles:extended-syntax:1.1", null, null)
		artifact.file = new File("src/test/resources/extended-syntax-tile.xml")
		assert participant.loadModel(artifact)
		artifact.file = new File("src/test/resources/session-license-tile.xml")
		assert participant.loadModel(artifact)
		artifact.file = new File("src/test/resources/bad-smelly-tile.xml")
		assert participant.loadModel(artifact)

		shouldFail(MavenExecutionException) {
			artifact.file = new File("src/test/resources/extended-syntax-tile1.xml")
			participant.loadModel(artifact)
		}

		shouldFail(MavenExecutionException) {
			artifact.file = new File("src/test/resources/invalid-tile.xml")
			participant.loadModel(artifact)
		}

		shouldFail(MavenExecutionException) {
			artifact.file = new File("src/test/resources/not-a-file-file.xml")
			participant.loadModel(artifact)
		}
	}

	@Test
	public void canUseModelResolver() {
		File licensePom = new File('src/test/resources/session-license-pom.xml')

		participant.mavenVersionIsolate = [
			resolveVersionRange: { Artifact artifact ->
				artifact.file = licensePom
			}
		] as MavenVersionIsolator

		def resolver = participant.createModelResolver()
		def model = resolver.resolveModel('my', 'left', 'foot')

		assert model.inputStream.text == licensePom.text
		assert model.location == licensePom.absolutePath

		model.inputStream.close()
	}

	protected Model readModel(File pomFile) {
		MavenXpp3Reader modelReader = new MavenXpp3Reader()
		Model pomModel

		pomFile.withReader { Reader r ->
			pomModel = modelReader.read(r)
		}

		return pomModel
	}

	@Test
	public void injectModelLayerTiles() {
		TileModel sessionLicenseTile = new TileModel(new File('src/test/resources/session-license-tile.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:session-license:1', null, null))

		TileModel extendedSyntaxTile = new TileModel(new File('src/test/resources/extended-syntax-tile.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:extended-syntax:1', null, null))

		TileModel antrunTile = new TileModel(new File('src/test/resources/antrun1-tile.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:antrun1:1', null, null))

		List<TileModel> tiles = [
			sessionLicenseTile,
			extendedSyntaxTile,
			antrunTile,
		]

		File pomFile = new File('src/test/resources/empty-pom.xml')
		Model pomModel = readModel(pomFile)

		Map<String, Model> modelCache = new HashMap<>()
		participant = new TilesMavenLifecycleParticipant() {
					@Override
					protected void putModelInCache(ModelBuilder modelBuilder, Model model, ModelBuildingRequest request, File pFile) {
					}
					@Override
					protected void putOriginalModelInCache(String groupId, String artifactId, String version, Model model, File pFile) {
						modelCache.put(groupId + ':' + artifactId, model)
					}
				}

		stuffParticipant()

		participant.injectTilesIntoParentStructure(null,
				[getGroupId: { pomModel.groupId }, getArtifactId: { pomModel.artifactId }, getProperties: { new Properties() } ] as MavenProject,
				tiles, pomModel, [getPomFile: { return pomFile } ] as ModelBuildingRequest)

		assert pomModel.parent.artifactId == 'empty-pom-tiles'
		assert pomModel.properties.".applied-tiles" == "io.repaint.tiles:session-license:1,io.repaint.tiles:extended-syntax:1,io.repaint.tiles:antrun1:1"
		assert modelCache.get('io.repaint.tiles:empty-pom-tiles')?.parent == null

		pomModel.parent = new Parent(groupId: 'io.repaint.tiles', artifactId: 'fake-parent', version: '1')

		participant.injectTilesIntoParentStructure(null,
				[getGroupId: { pomModel.groupId }, getArtifactId: { pomModel.artifactId }, getProperties: { new Properties() } ] as MavenProject,
				tiles, pomModel, [getPomFile: { return pomFile } ] as ModelBuildingRequest)
		assert modelCache.get('io.repaint.tiles:empty-pom-tiles')?.parent?.artifactId == 'fake-parent'
	}

	@Test
	public void testMergeProfilesFileGood() {
		// two tiles with the same profile with the same activation is OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-file-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-file-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-file-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-file-tile2:1', null, null))
		
		performMergeProfiles(tile1, tile2)
	}
	
	@Test
	public void testMergeProfilesFileBad() {
		// two tiles with the same profile but with different activations is NOT OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-file-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-file-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-file-tile2.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-file-tile2:1', null, null))
				
		Throwable failure = shouldFail(MavenExecutionException) {
			performMergeProfiles(tile1, tile2)
		}
		assert failure.message.contains("Activation File 1: file2")
		assert failure.message.contains("Activation File 2: file1")
	}
	
	@Test
	public void testMergeProfilesPropertyGood() {
		// two tiles with the same profile with the same activation is OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-property-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-property-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-property-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-property-tile2:1', null, null))
				
		performMergeProfiles(tile1, tile2)
	}
	
	@Test
	public void testMergeProfilesPropertyBad() {
		// two tiles with the same profile but with different activations is NOT OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-property-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-property-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-property-tile2.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-property-tile2:1', null, null))
				
		Throwable failure = shouldFail(MavenExecutionException) {
			performMergeProfiles(tile1, tile2)
		}
		assert failure.message.contains("Activation Property 1: property2")
		assert failure.message.contains("Activation Property 2: property1")
	}
	
	@Test
	public void testMergeProfilesJdkGood() {
		// two tiles with the same profile with the same activation is OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-jdk-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-jdk-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-jdk-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-jdk-tile2:1', null, null))
				
		performMergeProfiles(tile1, tile2)
	}
	
	@Test
	public void testMergeProfilesJdkBad() {
		// two tiles with the same profile but with different activations is NOT OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-jdk-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-jdk-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-jdk-tile2.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-jdk-tile2:1', null, null))
				
		Throwable failure = shouldFail(MavenExecutionException) {
			performMergeProfiles(tile1, tile2)
		}
		assert failure.message.contains("Activation JDK 1: [1.1,1.6)")
		assert failure.message.contains("Activation JDK 2: [1.8,1.11)")
	}
	
	
	@Test
	public void testMergeProfilesOSGood() {
		// two tiles with the same profile with the same activation is OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-os-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-os-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-os-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-os-tile2:1', null, null))
				
		performMergeProfiles(tile1, tile2)
	}
	
	@Test
	public void testMergeProfilesOSBad() {
		// two tiles with the same profile but with different activations is NOT OK
		TileModel tile1 = new TileModel(new File('src/test/resources/mergeprofiles/activation-os-tile1.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-os-tile1:1', null, null))
				
		TileModel tile2 = new TileModel(new File('src/test/resources/mergeprofiles/activation-os-tile2.xml'),
				participant.turnPropertyIntoUnprocessedTile('io.repaint.tiles:activation-os-tile2:1', null, null))
				
		Throwable failure = shouldFail(MavenExecutionException) {
			performMergeProfiles(tile1, tile2)
		}
		assert failure.message.contains("Activation OS 1: name: 'Windows 10', family: 'Windows', arch: 'x86', version: 'null'")
		assert failure.message.contains("Activation OS 2: name: 'Windows 8.1', family: 'Windows', arch: 'x86_64', version: '5.1.2600'")
	}
	
	private void performMergeProfiles(TileModel... tileModels) {
		List<TileModel> tiles = Arrays.asList(tileModels);
				                        		 
		File pomFile = new File('src/test/resources/empty-pom.xml')
		Model pomModel = readModel(pomFile)
		 
		Map<String, Model> modelCache = new HashMap<>()
		participant = new TilesMavenLifecycleParticipant() {
			@Override
			protected void putModelInCache(ModelBuilder modelBuilder, Model model, ModelBuildingRequest request, File pFile) {
			}
			@Override
			protected void putOriginalModelInCache(String groupId, String artifactId, String version, Model model, File pFile) {
				modelCache.put(groupId + ':' + artifactId, model)
			}
		}
		
		stuffParticipant()

		participant.injectTilesIntoParentStructure(null,
					[getGroupId: { pomModel.groupId }, getArtifactId: { pomModel.artifactId }, getProperties: { new Properties() } ] as MavenProject,
					tiles, pomModel, [getPomFile: { return pomFile } ] as ModelBuildingRequest)
	}
	
	@Test
	public void testNoTiles() throws MavenExecutionException {
		participant = new TilesMavenLifecycleParticipant() {
					@Override
					protected TileModel loadModel(Artifact artifact) throws MavenExecutionException {
						return new TileModel(model:new Model())
					}
				}

		stuffParticipant()

		MavenProject project = new MavenProject()
		MavenSession session = fakeSessionForProject(project)
		participant.orchestrateMerge(session, project)
	}

	@Test
	public void testBadGav() {
		Model model = createBasicModel()
		addTileAndPlugin(model, "groupid:artifactid")

		participant = new TilesMavenLifecycleParticipant()
		stuffParticipant()

		MavenProject project = new MavenProject(model)
		MavenSession session = fakeSessionForProject(project)

		Throwable failure = shouldFail {
			participant.orchestrateMerge(session, project)
		}

		assert failure.message == "groupid:artifactid does not have the form group:artifact:version-range or group:artifact:extension:classifier:version-range"
	}

	public void addTileAndPlugin(Model model, String gav) {
		// add our plugin
		model.build = new Build()
		model.build.addPlugin(new Plugin())
		model.build.plugins[0].with {
			groupId = TilesMavenLifecycleParticipant.TILEPLUGIN_GROUP
			artifactId = TilesMavenLifecycleParticipant.TILEPLUGIN_ARTIFACT
			// bad GAV
			configuration = Xpp3DomBuilder.build(new StringReader("<configuration><tiles><tile>${gav}</tile></tiles></configuration>"))
		}
	}

	protected resetParticipantToLoadTilesFromDisk(String folder = "src/test/resources") {
		participant = new TilesMavenLifecycleParticipant() {
					@Override
					protected void thunkModelBuilder(MavenSession mavenSession, MavenProject project) {
					}

					@Override
					protected Artifact resolveTile(MavenSession mavenSession, MavenProject project,  Artifact tileArtifact) throws MavenExecutionException {
						tileArtifact.file = new File(folder + "/" + "${tileArtifact.artifactId}.xml")

						return tileArtifact
					}
				}

		stuffParticipant()
	}

	protected MavenProject fakeProjectFromFile(String pom) {
		File pomFile = new File("src/test/resources/${pom}.xml")

		return [
			getModel: { return readModel(pomFile)},
			getPomFile: { return pomFile }
		] as MavenProject
	}

	protected MavenSession fakeSessionForProject(MavenProject project) {
		DefaultRepositorySystemSession repoSystemSession = new DefaultRepositorySystemSession()
		repoSystemSession.cache = new DefaultRepositoryCache()
		MavenSession session = new MavenSession(null, repoSystemSession,
				new DefaultMavenExecutionRequest(), null)
		session.setProjects([project])
		session.repositorySession.getData().set("$TileData", new TileData())
		return session;
	}

	@Test
	public void testTileResolve() {
		MavenProject project = fakeProjectFromFile("full-tile-load-pom")
		MavenSession session = fakeSessionForProject(project)
		resetParticipantToLoadTilesFromDisk()
		
		participant.orchestrateMerge(session, project)

		assert TilesMavenLifecycleParticipant.getTileData(session).processedTiles.size() == 4
	}

	@Test
	public void testDuplicateTilesIgnored() {
		MavenProject project = fakeProjectFromFile("duplicate-tile-pom")
		MavenSession session = fakeSessionForProject(project)

		resetParticipantToLoadTilesFromDisk()
		
		participant.orchestrateMerge(session, project)
		assert TilesMavenLifecycleParticipant.getTileData(session).processedTiles.size() == 4
	}

	@Test
	public void testRemoveTiles() {
		MavenProject project = fakeProjectFromFile("remove-tile-pom")
		MavenSession session = fakeSessionForProject(project)

		resetParticipantToLoadTilesFromDisk()

		participant.orchestrateMerge(session, project)
		assert TilesMavenLifecycleParticipant.getTileData(session).processedTiles.size() == 0
	}

	protected Model createBasicModel() {
		Model model = new Model()

		model.setGroupId("com.bluetrainsoftware.maven")
		model.setArtifactId("maven-tiles-example")
		model.setVersion("1.1-SNAPSHOT")

		Properties model1Properties = new Properties()
		model1Properties.setProperty("property1", "property1")
		model.setProperties(model1Properties)
		return model
	}
}
