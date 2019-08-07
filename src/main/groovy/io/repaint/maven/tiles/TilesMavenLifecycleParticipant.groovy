/*
 * **********************************************************************************************************************
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

import static io.repaint.maven.tiles.GavUtil.artifactGav
import static io.repaint.maven.tiles.GavUtil.artifactName
import static io.repaint.maven.tiles.GavUtil.modelGa
import static io.repaint.maven.tiles.GavUtil.modelGav
import static io.repaint.maven.tiles.GavUtil.modelRealGa
import static io.repaint.maven.tiles.GavUtil.parentGav

import org.apache.maven.AbstractMavenLifecycleParticipant
import org.apache.maven.MavenExecutionException
import org.apache.maven.artifact.Artifact
import org.apache.maven.artifact.DefaultArtifact
import org.apache.maven.artifact.handler.DefaultArtifactHandler
import org.apache.maven.artifact.repository.ArtifactRepository
import org.apache.maven.artifact.repository.ArtifactRepositoryFactory
import org.apache.maven.artifact.repository.layout.ArtifactRepositoryLayout
import org.apache.maven.artifact.resolver.ArtifactNotFoundException
import org.apache.maven.artifact.resolver.ArtifactResolutionException
import org.apache.maven.artifact.resolver.ArtifactResolver
import org.apache.maven.artifact.versioning.VersionRange
import org.apache.maven.execution.MavenSession
import org.apache.maven.model.Build
import org.apache.maven.model.Dependency
import org.apache.maven.model.DistributionManagement
import org.apache.maven.model.Exclusion
import org.apache.maven.model.InputLocation
import org.apache.maven.model.Model
import org.apache.maven.model.Parent
import org.apache.maven.model.Plugin
import org.apache.maven.model.PluginManagement
import org.apache.maven.model.Profile;
import org.apache.maven.model.ReportPlugin
import org.apache.maven.model.ReportSet
import org.apache.maven.model.Repository
import org.apache.maven.model.Resource
import org.apache.maven.model.building.DefaultModelBuilder
import org.apache.maven.model.building.DefaultModelBuilderFactory
import org.apache.maven.model.building.DefaultModelBuildingRequest
import org.apache.maven.model.building.ModelBuilder
import org.apache.maven.model.building.ModelBuildingListener
import org.apache.maven.model.building.ModelBuildingRequest
import org.apache.maven.model.building.ModelBuildingResult
import org.apache.maven.model.building.ModelProblemCollector
import org.apache.maven.model.building.ModelProblemCollectorRequest
import org.apache.maven.model.building.ModelProcessor
import org.apache.maven.model.building.ModelSource2
import org.apache.maven.model.inheritance.DefaultInheritanceAssembler
import org.apache.maven.model.inheritance.InheritanceAssembler
import org.apache.maven.model.io.ModelParseException
import org.apache.maven.model.io.ModelWriter
import org.apache.maven.model.normalization.DefaultModelNormalizer
import org.apache.maven.model.normalization.ModelNormalizer
import org.apache.maven.model.plugin.LifecycleBindingsInjector
import org.apache.maven.model.profile.DefaultProfileActivationContext
import org.apache.maven.model.profile.ProfileSelector
import org.apache.maven.model.resolution.InvalidRepositoryException
import org.apache.maven.model.resolution.ModelResolver
import org.apache.maven.model.resolution.UnresolvableModelException
import org.apache.maven.project.DefaultProjectBuilder
import org.apache.maven.project.DefaultProjectBuildingRequest
import org.apache.maven.project.MavenProject
import org.apache.maven.project.ProjectBuilder
import org.apache.maven.project.ProjectBuildingHelper
import org.apache.maven.project.ProjectBuildingRequest
import org.apache.maven.project.ProjectBuildingResult
import org.apache.maven.repository.RepositorySystem
import org.apache.maven.shared.filtering.MavenFileFilter
import org.apache.maven.shared.filtering.MavenFileFilterRequest
import org.apache.maven.shared.filtering.MavenResourcesExecution
import org.apache.maven.shared.filtering.MavenResourcesFiltering
import org.codehaus.plexus.component.annotations.Component
import org.codehaus.plexus.component.annotations.Requirement
import org.codehaus.plexus.interpolation.PropertiesBasedValueSource
import org.codehaus.plexus.interpolation.StringSearchInterpolator
import org.codehaus.plexus.logging.Logger
import org.codehaus.plexus.util.xml.Xpp3Dom
import org.codehaus.plexus.util.xml.pull.XmlPullParserException
import org.eclipse.aether.RepositoryCache
import org.eclipse.aether.RepositorySystemSession
import org.xml.sax.SAXParseException

import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode
import io.repaint.maven.tiles.TilesMavenLifecycleParticipant.CachingModelSource
import io.repaint.maven.tiles.isolators.AetherIsolator
import io.repaint.maven.tiles.isolators.Maven30Isolator
import io.repaint.maven.tiles.isolators.Maven35Isolator
import io.repaint.maven.tiles.isolators.MavenVersionIsolator


/**
 * Fetches all dependencies defined in the POM `configuration`.
 *
 * Merging operation is delegated to {@link DefaultModelBuilder}
 *
 * @author: Richard Vowles - https://plus.google.com/+RichardVowles
 * @author: Mark Derricutt - https://plus.google.com/+MarkDerricutt
 *
 */
@CompileStatic
@Component(role = AbstractMavenLifecycleParticipant, hint = "TilesMavenLifecycleParticipant")
public class TilesMavenLifecycleParticipant extends AbstractMavenLifecycleParticipant {

	protected static final String TILE_EXTENSION = 'pom'
	public static final TILEPLUGIN_GROUP = 'io.repaint.maven'
	public static final TILEPLUGIN_ARTIFACT = 'tiles-maven-plugin'
	public static final String TILEPLUGIN_KEY = "${TILEPLUGIN_GROUP}:${TILEPLUGIN_ARTIFACT}"

	@Requirement
	Logger logger

	@Requirement
	ModelWriter modelWriter

	@Requirement
	ArtifactResolver resolver

	@Requirement
	ModelProcessor modelProcessor

	@Requirement
	ProjectBuildingHelper projectBuildingHelper

	@Requirement
	MavenFileFilter mavenFileFilter

	@Requirement
	MavenResourcesFiltering mavenResourcesFiltering

	@Requirement
	ProjectBuilder projectBuilder

	@Requirement
	ProfileSelector profileSelector

	@Requirement
	LifecycleBindingsInjector lifecycleBindingsInjector

	@Requirement
	RepositorySystem repositorySystem

	/**
	 * Component used to create a repository.
	 */
	ArtifactRepositoryFactory repositoryFactory

	/**
	 * Map that contains the layouts.
	 */
	@Requirement( role = ArtifactRepositoryLayout.class )
	private Map<String, ArtifactRepositoryLayout> repositoryLayouts

	protected MavenVersionIsolator mavenVersionIsolate

	List<ArtifactRepository> remoteRepositories
	ArtifactRepository localRepository

	NotDefaultModelCache modelCache

	MavenSession mavenSession

	class ArtifactModel {
		public Artifact artifact
		public TileModel tileModel

		public ArtifactModel(Artifact artifact, TileModel tileModel) {
			this.artifact = artifact
			this.tileModel = tileModel
		}
	}

	/**
	 * We store the groupId:artifactId of the parent where to apply tiles on. If not specified the tiles will be
	 * applied on (as parents of) each module building.
	 */
	String applyBeforeParent

	/**
	 * We store the groupId:artifactId of the parent where to merge tiles on. If not specified the tiles will be
	 * merged into either the parent specified by applyBeforeParent or on each module building if empty.
	 */
	String mergeTarget

	/**
	 * This specifically goes and asks the repository for the "tile" attachment for this pom, not the
	 * pom itself (because we don't care about that).
	 */
	protected Artifact getArtifactFromCoordinates(String groupId, String artifactId, String type, String classifier, String version) {
		return new DefaultArtifact(groupId, artifactId, VersionRange.createFromVersion(version), "compile",
				type, classifier, new DefaultArtifactHandler(type))
	}

	/**
	 * Return the given Artifact's .pom artifact
	 */
	protected Artifact getPomArtifactForArtifact(Artifact artifact) {
		return getArtifactFromCoordinates(artifact.groupId, artifact.artifactId, 'pom', '', artifact.version)
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected File getTileFromProject(MavenSession mavenSession, MavenProject tileProject) {
		Xpp3Dom configuration = tileProject.build?.plugins?.
				find({ Plugin plugin ->
					return plugin.groupId == TILEPLUGIN_GROUP &&
							plugin.artifactId == TILEPLUGIN_ARTIFACT
				})?.configuration as Xpp3Dom

		File baseTile = new File(tileProject.basedir, AbstractTileMojo.TILE_POM)
		if (configuration?.getChild("filtering")?.value == "true") {
			// tile.xml needs to be filtered
			File processedTileDirectory = new File(tileProject.build.directory + "/generated-sources", "tiles")
			File processedTile = new File(processedTileDirectory, AbstractTileMojo.TILE_POM)

			Resource tileResource = new Resource()
			tileResource.setDirectory(tileProject.basedir.absolutePath)
			tileResource.includes.add(AbstractTileMojo.TILE_POM)
			tileResource.setFiltering(true)

			MavenFileFilterRequest req = new MavenFileFilterRequest(baseTile, processedTile, true,
					tileProject, [], true, "UTF-8", mavenSession, new Properties())
			req.setDelimiters(["@"] as LinkedHashSet)

			MavenResourcesExecution execution = new MavenResourcesExecution(
					[tileResource], processedTileDirectory, "UTF-8",
					mavenFileFilter.getDefaultFilterWrappers(req),
					tileProject.basedir, mavenResourcesFiltering.defaultNonFilteredFileExtensions)

			Logger old = mavenResourcesFiltering.logger
			try {
				mavenResourcesFiltering.enableLogging(new DummyLogger(mavenResourcesFiltering.logger))
				mavenResourcesFiltering.filterResources(execution)
			} finally {
				mavenResourcesFiltering.enableLogging(old)
			}

			return new File(processedTileDirectory, AbstractTileMojo.TILE_POM)
		} else {
			return new File(tileProject.file.parent, AbstractTileMojo.TILE_POM)
		}
	}

	protected Artifact resolveTile(MavenSession mavenSession, MavenProject project, Artifact tileArtifact) throws MavenExecutionException {
		// try to find tile from reactor
		if (mavenSession != null) {
			List<MavenProject> allProjects = mavenSession.getProjects()
			if (allProjects != null) {
				for (MavenProject currentProject : allProjects) {
					// when loading from reactor ignore version
					if (currentProject.groupId == tileArtifact.groupId && currentProject.artifactId == tileArtifact.artifactId) {
						tileArtifact.version = currentProject.version
						tileArtifact.file = getTileFromProject(mavenSession, currentProject)
						return tileArtifact
					}
				}
			}
		}

		try {
			mavenVersionIsolate.resolveVersionRange(tileArtifact)
			if (tileArtifact.version.startsWith('$') && project) {
				// get version from project properties
				StringSearchInterpolator interpolator = new StringSearchInterpolator()
				interpolator.addValueSource(new PropertiesBasedValueSource(project.properties))
				tileArtifact.version = interpolator.interpolate(tileArtifact.version)
				logger.debug("Interpolated tile ${GavUtil.artifactGav(tileArtifact)} version as ${tileArtifact.version}")
			}

			// Resolve the .xml file for the tile
			resolver.resolve(tileArtifact, remoteRepositories, localRepository)

			// When resolving from workspace (e.g. m2e) we might receive the path to pom.xml instead of the attached tile
			if (tileArtifact.file && tileArtifact.file.name == "pom.xml") {
				// to enable filtering we need to create a project first
				RepositoryCache cache = mavenSession.getRepositorySession().getCache()
				File tileEffective = (File) cache.get(mavenSession.getRepositorySession(), "tile:" + tileArtifact.file)
				if (!tileEffective
						|| !tileEffective.exists()
						|| tileEffective.lastModified() < tileArtifact.file.lastModified()) {
					ProjectBuildingRequest prjRequest = new DefaultProjectBuildingRequest(mavenSession.projectBuildingRequest)
					prjRequest.project = null
					prjRequest.setResolveDependencies(false)
					prjRequest.userProperties = mavenSession.userProperties
					prjRequest.systemProperties = mavenSession.systemProperties
					prjRequest.profiles = mavenSession.request.profiles
					prjRequest.activeProfileIds = mavenSession.request.projectBuildingRequest.activeProfileIds
					prjRequest.inactiveProfileIds = mavenSession.request.projectBuildingRequest.inactiveProfileIds
					ProjectBuildingResult prjResult = projectBuilder.build(tileArtifact.file, prjRequest)
					// project building might be expensive, so cache it in a way that will cache it also for m2e
					tileEffective = getTileFromProject(mavenSession, prjResult.project)
					cache.put(mavenSession.getRepositorySession(), "tile:" + tileArtifact.file, tileEffective)
				}
				tileArtifact.file = tileEffective

				if (!tileArtifact.file.exists()) {
					throw new MavenExecutionException("Tile ${artifactGav(tileArtifact)} cannot be resolved.",
					tileArtifact.getFile())
				}
			}

			// Resolve the .pom file for the tile
			Artifact pomArtifact = getPomArtifactForArtifact(tileArtifact)
			resolver.resolve(pomArtifact, remoteRepositories, localRepository)

			if (System.getProperty("performRelease")?.asBoolean()) {

				if (tileArtifact.version.endsWith("-SNAPSHOT")) {

					throw new MavenExecutionException("Tile ${artifactGav(tileArtifact)} is a SNAPSHOT and we are releasing.",
					tileArtifact.getFile())

				}
			}

		} catch (ArtifactResolutionException e) {
			throw new MavenExecutionException(e.getMessage(), e)
		} catch (ArtifactNotFoundException e) {
			throw new MavenExecutionException(e.getMessage(), e)
		}

		return tileArtifact
	}
	protected String[] tokenizeWithProperties(String value) {
		List<String> tokens = new ArrayList()
		int inProperty = 0
		StringBuilder currentValue = new StringBuilder()
		next: for (int i=0; i<value.length(); ++i) {
			char c = value.charAt(i)
			switch (c) {
				case '$':
					if ((i+1) < value.length() && value.charAt(i+1) == '{') {
						++inProperty
						++i
						currentValue.append('${')
						continue next
					}
					break

				case '}':
					if (inProperty > 0) {
						inProperty--
					}
					break

				case ':':
					if (inProperty <= 0) {
						tokens.add(currentValue.toString())
						currentValue = new StringBuilder()
						continue next
					}
					break

				default:
					break
			}
			currentValue.append(c)
		}
		if (currentValue.size() > 0) {
			tokens.add(currentValue.toString())
		}
		return tokens.toArray(new String[tokens.size()])
	}

	protected Artifact turnPropertyIntoUnprocessedTile(String artifactGav, MavenProject project, File pomFile)
	throws MavenExecutionException {

		String[] gav = tokenizeWithProperties(artifactGav)
		if (gav.size() == 2 || gav.size() == 4) {
			if (project?.managedVersionMap) {
				String key = gav[0] + ":" + gav[1] + ":xml"
				Artifact managedArtifact = project.managedVersionMap.get(key);
				if (managedArtifact) {
					return managedArtifact;
				} else {
					throw new MavenExecutionException("${artifactGav} is not a managedDependency for $key", pomFile)
				}
			} else {
				logger.debug("${artifactGav} has no project context")
			}
		}

		if (gav.size() != 3 && gav.size() != 5) {
			throw new MavenExecutionException("${artifactGav} does not have the form group:artifact:version-range or group:artifact:extension:classifier:version-range", pomFile)
		}

		String groupId = gav[0]
		String artifactId = gav[1]
		String version
		String type = "xml"
		String classifier = ""
		if (gav.size() == 3) {
			version = gav[2]
		} else {
			type = gav[2]
			classifier = gav[3]
			version = gav[4]
		}

		return getArtifactFromCoordinates(groupId, artifactId, type, classifier, version)
	}

	protected TileModel loadModel(Artifact artifact) throws MavenExecutionException {
		try {
			TileModel tileModel = getTileModelFromCache(artifact.groupId, artifact.artifactId, artifact.version)
			if (!tileModel) {
				tileModel = new TileModel(artifact.getFile(), artifact)
				putTileModelInCache(tileModel)
			}

			logger.debug(String.format("Loaded Maven Tile %s", artifactGav(artifact)))

			return tileModel

		} catch (FileNotFoundException e) {
			throw new MavenExecutionException(String.format("Error loading %s", artifactGav(artifact)), e)
		} catch (XmlPullParserException e) {
			throw new MavenExecutionException(String.format("Error building %s", artifactGav(artifact)), e)
		} catch (SAXParseException sax) {
			throw new MavenExecutionException(String.format("Error building %s", artifactGav(artifact)), sax)
		} catch (IOException e) {
			throw new MavenExecutionException(String.format("Error parsing %s", artifactGav(artifact)), e)
		}
	}

	protected MavenVersionIsolator discoverMavenVersion(MavenSession mavenSession) {
		MavenVersionIsolator isolator

		try {
			isolator = new Maven35Isolator(mavenSession)
		} catch (MavenExecutionException mee) {
			try {
				isolator = new AetherIsolator(mavenSession)
			} catch (MavenExecutionException mee2) {
				isolator = new Maven30Isolator(mavenSession)
			}
		}

		return isolator
	}

	/**
	 * Invoked after all MavenProject instances have been created.
	 *
	 * This callback is intended to allow extensions to manipulate MavenProjects
	 * before they are sorted and actual build execution starts.
	 */
	public void afterProjectsRead(MavenSession mavenSession)
	throws MavenExecutionException {

		this.mavenSession = mavenSession

		this.remoteRepositories = mavenSession.request.remoteRepositories
		this.localRepository = mavenSession.request.localRepository

		// get the activate profiles from settings according to activation rules now
		DefaultProfileActivationContext context = new DefaultProfileActivationContext()
		context.activeProfileIds = mavenSession.request.projectBuildingRequest.activeProfileIds
		context.inactiveProfileIds = mavenSession.request.projectBuildingRequest.inactiveProfileIds
		context.systemProperties = mavenSession.request.systemProperties
		context.userProperties = mavenSession.request.userProperties
		context.projectDirectory = mavenSession.request.pom ? mavenSession.request.pom.parentFile : (File) null

		// manually add repositories from these profiles (if not activated yet)
		List<Profile> activeExternalProfiles = profileSelector.getActiveProfiles(mavenSession.request.profiles, context,
			new ModelProblemCollector() {
				@Override
				public void add(ModelProblemCollectorRequest request) {
					// ignore
				}
			})
		for (Profile profile : activeExternalProfiles) {
			if (!mavenSession.request.projectBuildingRequest.activeProfileIds.contains(profile.id)) {
				logger.info("Activating profile from settings: " + profile.id)
				for (Repository remoteRepository : profile.repositories)
				{
					ArtifactRepository repo = repositorySystem.buildArtifactRepository( remoteRepository )
					repositorySystem.injectAuthentication(mavenSession.repositorySession, Collections.singletonList(repo))
					remoteRepositories.add(repo)
				}
			}
		}

		this.mavenVersionIsolate = discoverMavenVersion(mavenSession)

		repositoryFactory = mavenSession.container.lookup(ArtifactRepositoryFactory)
		repositoryLayouts = mavenSession.lookupMap(ArtifactRepositoryLayout.class.getName()) as Map<String, ArtifactRepositoryLayout>

		List<MavenProject> allProjects = mavenSession.getProjects()
		if (allProjects != null) {
			Set<String> parentsAppliedWithTiles = new HashSet<String>()
			for (MavenProject currentProject : allProjects) {

				List<String> subModules = currentProject.getModules()
				boolean containsTiles = currentProject.getPluginArtifactMap().keySet().contains(TILEPLUGIN_KEY)

				if (containsTiles) {
					TileData tileData = new TileData()
					def oldTileData = mavenSession.repositorySession.getData().get("$TileData")
					try {
						mavenSession.repositorySession.getData().set("$TileData", tileData)
						orchestrateMerge(mavenSession, currentProject)
						if (!parentsAppliedWithTiles.empty && (!applyBeforeParent || !parentsAppliedWithTiles.contains(applyBeforeParent))) {
							//applyBeforeParent must not be set to different parents in a reactor build. We would end up
							//with different tiles applied in multiple positions in a parent hierarchy if that parent
							//is a parent of the current module, too. We allow it if no explicit parent was specified and
							//the previously specified parent is not in the hierarchy
							for (MavenProject checkParent = currentProject.parent; checkParent != null; checkParent = checkParent.parent) {
								if (parentsAppliedWithTiles.contains(modelGa(checkParent.model))) {
									throw new MavenExecutionException("<applyBefore>${modelGa(checkParent.model)}</applyBefore> has already been used in another module and it's a parent of this module, too, so you also need to use it for this module.", currentProject.getFile())
								}
							}
						}
						if (applyBeforeParent) {
							parentsAppliedWithTiles.add(applyBeforeParent)
						}

						// did we expect but not get a distribution artifact repository?
						if (!currentProject.distributionManagementArtifactRepository) {
							discoverAndSetDistributionManagementArtifactoryRepositoriesIfTheyExist(currentProject)
						}

					} finally {
						// restore previous tile data
						mavenSession.repositorySession.getData().set("$TileData", oldTileData)
					}
				}
			}
		}
	}

	/**
	 * If we get here, we have a Tiles project that might have a distribution management section but it is playing
	 * dicky-birds and hasn't set up the distribution management repositories.
	 *
	 * @param project
	 */
	void discoverAndSetDistributionManagementArtifactoryRepositoriesIfTheyExist(MavenProject project) {
		DistributionManagement distributionManagement = project.model.distributionManagement

		if (distributionManagement) {
			if (distributionManagement.repository) {
				project.setReleaseArtifactRepository(repositoryFactory.createDeploymentArtifactRepository(
						distributionManagement.repository.id, distributionManagement.repository.url,
						repositoryLayouts.get( distributionManagement.repository.layout ?: 'default' ), true ))
			}
			if (distributionManagement.snapshotRepository) {
				project.setSnapshotArtifactRepository(repositoryFactory.createDeploymentArtifactRepository(
						distributionManagement.snapshotRepository.id, distributionManagement.snapshotRepository.url,
						repositoryLayouts.get( distributionManagement.snapshotRepository.layout ?: 'default' ), true ))
			}
		}
	}

	/**
	 * Merges the files over the top of the project, and then the individual project back over the top.
	 * The reason for this is that the super pom and packaging can set plugin versions. This allows the tiles
	 * to overwrite those, and then if they are manually specified in the pom, they then get set again.
	 * @param project - the currently evaluated project
	 * @throws MavenExecutionException
	 */
	protected void orchestrateMerge(MavenSession mavenSession, MavenProject project) throws MavenExecutionException {
		// Clear collected tiles from previous project in reactor
		this.modelCache = new NotDefaultModelCache(mavenSession.repositorySession)

		// collect the first set of tiles
		parseConfiguration(mavenSession, project, project.model, project.getFile(), true)

		// collect any unprocessed tiles, and process them causing them to potentially load more unprocessed ones
		loadAllDiscoveredTiles(mavenSession, project)

		// don't do anything if there are no tiles
		if (getTileData(mavenSession).processedTiles) {
			thunkModelBuilder(mavenSession, project)
		}
	}

	/**
	 * Get the TileData from the passed MavenSession. It will be put there in orchestrateMerge
	 * @param mavenSession
	 * @return
	 */
	protected static TileData getTileData(MavenSession mavenSession) throws MavenExecutionException {
		TileData tileData = (TileData) mavenSession.repositorySession.getData().get("$TileData")
		if (tileData == null) {
			throw new MavenExecutionException("No tiles associated with current MavenSession.",
			mavenSession.currentProject.file)
		}
		return tileData
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected void thunkModelBuilder(MavenSession mavenSession, MavenProject project) {
		TileData tileData = getTileData(mavenSession)
		List<TileModel> tiles = tileData.processedTiles.values().collect({it.tileModel})

		if (!tiles) return

			// Maven 3.2.5 doesn't let you have access to this (package private), 3.3.x does
			def modelBuildingListenerConstructor = Class.forName("org.apache.maven.project.DefaultModelBuildingListener").declaredConstructors[0]
		modelBuildingListenerConstructor.accessible = true
		ModelBuildingListener modelBuildingListener = modelBuildingListenerConstructor.newInstance(project,
				projectBuildingHelper, mavenSession.request.projectBuildingRequest)

		// new org.apache.maven.project.PublicDefaultModelBuildingListener( project,
		//projectBuildingHelper, mavenSession.request.projectBuildingRequest )
		// this allows us to know when the ModelProcessor is called that we should inject the tiles into the
		// parent structure
		ModelSource2 mainArtifactModelSource = new CachingModelSource(project.artifact, project.file)

		ModelBuildingRequest request = new DefaultModelBuildingRequest(modelSource: mainArtifactModelSource,
				pomFile: project.file, modelResolver: createModelResolver(), modelCache: modelCache,
				systemProperties: mavenSession.request.systemProperties, userProperties: mavenSession.request.userProperties,
				profiles: mavenSession.request.projectBuildingRequest.profiles,
				activeProfileIds: mavenSession.request.projectBuildingRequest.activeProfileIds,
				inactiveProfileIds: mavenSession.request.projectBuildingRequest.inactiveProfileIds,
				modelBuildingListener: modelBuildingListener,
				locationTracking: true, twoPhaseBuilding: true, processPlugins: true)

		boolean tilesInjected = false
		boolean tilesMerged = false

		final DefaultModelBuilder modelBuilder = new DefaultModelBuilderFactory().newInstance()
		modelBuilder.setLifecycleBindingsInjector(lifecycleBindingsInjector)
		ModelProcessor delegateModelProcessor = new ModelProcessor() {
			@Override
			File locatePom(File projectDirectory) {
				return modelProcessor.locatePom(projectDirectory)
			}

			@Override
			Model read(File input, Map<String, ?> options) throws IOException, ModelParseException {
				return modelProcessor.read(input, options)
			}

			@Override
			Model read(Reader input, Map<String, ?> options) throws IOException, ModelParseException {
				return modelProcessor.read(input, options)
			}

			@Override
			Model read(InputStream input, Map<String, ?> options) throws IOException, ModelParseException {
				RepositoryCache cache = mavenSession.getRepositorySession().getCache()
				Model model
				if (options[ModelProcessor.SOURCE] instanceof CachingModelSource) {
					CachingModelSource source = (CachingModelSource) options[ModelProcessor.SOURCE]
					if (source) {
						model = source.getModel(modelProcessor, options)
					}
					input.close()
				} else {
					model = modelProcessor.read(input, options)
				}

				use(GavUtil) {
					if (model.artifactId == project.artifactId
					&& model.realGroupId == project.groupId
					&& model.realVersion == project.originalModel.realVersion
					&& model.packaging == project.packaging) {
						// we're at the first (project) level. Apply tiles here if no explicit parent is set
						if (!mergeTarget || modelRealGa(model) == mergeTarget) {
							mergeTilesInto(project, tiles, model)
							tilesMerged = true
						}
						if (!applyBeforeParent || modelRealGa(model) == applyBeforeParent) {
							injectTilesIntoParentStructure(modelBuilder, project, tiles, model, request)
							tilesInjected = true
						}
					} else if (modelRealGa(model) == applyBeforeParent) {
						// we're at the level with the explicitly selected parent. Apply the tiles here
						if (!tilesMerged) {
							// last level where we can merge
							mergeTilesInto(project, tiles, model)
							tilesMerged = true
						}
						if (!tilesInjected) {
							injectTilesIntoParentStructure(modelBuilder, project, tiles, model, request)
							tilesInjected = true
						}
					} else if (!tilesMerged && mergeTarget && modelRealGa(model) == mergeTarget) {
						// explicitly define merge level
						mergeTilesInto(project, tiles, model)
						tilesMerged = true
					} else if (model.packaging == 'tile' || model.packaging == 'pom') {
						// we could be at a parent that is a tile. In this case return the precomputed model
						TileModel oneOfUs = tiles.find { TileModel tm ->
							Model tileModel = tm.model
							return (model.artifactId == tileModel.artifactId && model.realGroupId == tileModel.realGroupId &&
									model.realVersion == tileModel.realVersion)
						}

						if (oneOfUs) {
							model = oneOfUs.model
						}
					}

					// if we want to apply tiles at a specific parent and have not come by it yet, we need
					// to make the parent reference project specific, so that it will not pick up a cached
					// version. We do this by clearing out any existing model from the cache.
					if(applyBeforeParent && !tilesInjected && model.parent) {
						// remove the parent from the cache which causes it to be reloaded through our ModelProcessor
						request.modelCache.put(model.parent.groupId, model.parent.artifactId, model.parent.version,
								org.apache.maven.model.building.ModelCacheTag.RAW.getName(), null)
					}
				}

				return model
			}
		}
		modelBuilder.setModelProcessor(delegateModelProcessor)

		ModelBuildingResult interimBuild = modelBuilder.build(request)

		ModelBuildingResult finalModel = modelBuilder.build(request, interimBuild)
		if (!tilesInjected && applyBeforeParent) {
			throw new MavenExecutionException("Cannot apply tiles for ${project.groupId}:${project.artifactId}, the expected parent ${applyBeforeParent} is not found.",
			project.file)
		}
		copyModel(project, finalModel.effectiveModel)
	}

	protected ModelResolver createModelResolver() {
		return new TilesProjectModelResolver(remoteRepositories)
	}

	class TilesProjectModelResolver implements ModelResolver {

		List<ArtifactRepository> effectiveRemoteRepositories

		TilesProjectModelResolver(List<ArtifactRepository> remoteRepositories) {
			effectiveRemoteRepositories = new ArrayList<>(remoteRepositories ?: new ArrayList<ArtifactRepository>())
		}

		@Override
		public void addRepository( final Repository repository, boolean replace )
			throws InvalidRepositoryException {
				if (replace) {
					effectiveRemoteRepositories.removeIf({ it.id == repository.id })
				} else {
					if (effectiveRemoteRepositories.stream().anyMatch({ it.id == repository.id })) {
						return
					}
				}
				ArtifactRepository repo = repositorySystem.buildArtifactRepository( repository )
				repositorySystem.injectAuthentication(mavenSession.repositorySession, Collections.singletonList(repo))
				effectiveRemoteRepositories.add(repo)
		}

		@Override
		public void addRepository( Repository repository )
			throws InvalidRepositoryException {
			addRepository(repository, false)
		}

		@Override
		ModelSource2 resolveModel(String groupId, String artifactId, String version) throws UnresolvableModelException {
			Artifact artifact = new DefaultArtifact(groupId, artifactId, VersionRange.createFromVersion(version), "compile",
					"pom", null, new DefaultArtifactHandler("pom"))

			mavenVersionIsolate.resolveVersionRange(artifact)
			TilesMavenLifecycleParticipant.this.resolver.resolve(artifact, effectiveRemoteRepositories, localRepository)

			return new CachingModelSource(artifact)
		}

		@Override
		ModelSource2 resolveModel(Parent parent) throws UnresolvableModelException {
			return resolveModel(parent.groupId, parent.artifactId, parent.version)
		}

		@Override
		ModelResolver newCopy() {
			return new TilesProjectModelResolver(effectiveRemoteRepositories)
		}
	}

	/**
	 * This is out of static type checking because the method is private and the class ModelCacheTag
	 * is package-private.
	 *
	 * @param model - the model we are inserting into the cache
	 * @param request - the building request, it holds the cache reference
	 * @param pomFile - the pomFile is required for model data for Maven 3.2.x not for 3.0.x
	 */
	@CompileStatic(TypeCheckingMode.SKIP)
	protected void putModelInCache(ModelBuilder modelBuilder, Model model, ModelBuildingRequest request, File pomFile) {
		// stuff it in the cache so it is ready when requested rather than it trying to be resolved.
		modelBuilder.putCache(request.modelCache, model.groupId, model.artifactId, model.version,
				org.apache.maven.model.building.ModelCacheTag.RAW,
				mavenVersionIsolate.createModelData(model, pomFile))
		//				new org.apache.maven.model.building.ModelData(new FileModelSource(tileModel.tilePom), model));
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected void putOriginalModelInCache(String groupId, String artifactId, String version, Model model, File file) {
		if (modelCache) {
			modelCache.putOriginal(groupId, artifactId, version, org.apache.maven.model.building.ModelCacheTag.RAW.name,
				mavenVersionIsolate.createModelData(model, file))
		}
	}

	protected void putOriginalModelInCache(Model model, File file) {
		putOriginalModelInCache(model.groupId, model.artifactId, model.version, model?.clone(), file)
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected Model getOriginalModelFromCache(String groupId, String artifactId, String version) {
		// stuff the original model in the original cache
		if (!modelCache) {
			return null
		}
		def modelData = modelCache.getOriginal(groupId, artifactId, version, org.apache.maven.model.building.ModelCacheTag.RAW.name)
		if (modelData) {
			return modelData.model.clone()
		}
		return null
	}

	protected void putTileModelInCache(TileModel model) {
		if (modelCache) {
			modelCache.putOriginal(model.model.groupId, model.model.artifactId, model.model.version, "tile", [model.tilePom, model.model, model.tiles, model.fragmentNames, model.fragmentModel])
		}
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected TileModel getTileModelFromCache(String groupId, String artifactId, String version) {
		if (!modelCache) {
			return null
		}
		// stuff the original model in the original cache
		def entry = modelCache.getOriginal(groupId, artifactId, version, "tile")
		if (entry)  {
			def (pom, model, tiles, fragmentNames, fragmentModel) = entry
			if (model) {
				return new TileModel(pom, model, tiles, fragmentNames, fragmentModel)
			}
		}
		return null
	}

	/**
	 * Creates a chain of tile parents based on how we discovered them and inserts them into the parent
	 * chain, above this project and before this project's parent (if any)
	 *
	 * @param tiles - tiles that should make up part of the collection
	 * @param pomModel - the current project
	 */
	@CompileStatic(TypeCheckingMode.SKIP)
	public void mergeTilesInto(MavenProject project, List<TileModel> tiles, Model pomModel) {
		if (tiles && tiles.count( { TileModel tileModel -> tileModel.fragmentModel } )) {
			logger.info("--- tiles-maven-plugin: Merging tiles for ${project.groupId}:${project.artifactId}...")
			tiles.reverseEach { TileModel tileModel ->
				// apply merged fragments
				if (tileModel.fragmentModel) {
					def merger = new org.apache.maven.model.profile.DefaultProfileInjector.ProfileModelMerger()
					merger.merge(pomModel, tileModel.fragmentModel.clone(), false, null)
					logger.debug("Merged '${modelGav(tileModel.model)}':${tileModel.fragmentNames} into '${modelGav(pomModel)}'.")
				}
			}
		}
	}

	/**
	 * Creates a chain of tile parents based on how we discovered them and inserts them into the parent
	 * chain, above this project and before this project's parent (if any)
	 *
	 * @param tiles - tiles that should make up part of the collection
	 * @param pomModel - the current project
	 * @param request - the request to build the current project
	 */
	@CompileStatic(TypeCheckingMode.SKIP)
	public void injectTilesIntoParentStructure(ModelBuilder modelBuilder, MavenProject project, List<TileModel> tiles, Model pomModel,
			ModelBuildingRequest request) {
		Parent originalParent = pomModel.parent
		Model lastPom = pomModel
		File lastPomFile = request.pomFile
		if (tiles) {
			logger.info("--- tiles-maven-plugin: Injecting ${tiles.size()} tiles as intermediary parent artifacts for ${project.groupId}:${project.artifactId}...")
			logger.debug("Mixed '${modelGav(pomModel)}' with tile '${modelGav(tiles.first().model)}' as it's new parent.")

			// if there is a parent make sure the inherited groupId / version is correct
			if (!pomModel.groupId) {
				pomModel.groupId = originalParent.groupId
				logger.info("Explicitly set groupId to '${pomModel.groupId}' from original parent '${parentGav(originalParent)}'.")
			}
			if (!pomModel.version) {
				pomModel.version = originalParent.version
				logger.info("Explicitly set version to '${pomModel.version}' from original parent '${parentGav(originalParent)}'.")
			}

			// original parent version might need to be interpolated
			if (originalParent?.version?.contains("\$")) {
				StringSearchInterpolator interpolator = new StringSearchInterpolator()
				interpolator.addValueSource(new PropertiesBasedValueSource(project.properties))
				originalParent.version = interpolator.interpolate(originalParent.version)
			}

			// cached id of combined tiles
			def tilesId = tiles.collect { tile -> tile.model.groupId + "_" + tile.model.artifactId + "_" + tile.model.version}.join("_")
			Model combinedTilesModel = getOriginalModelFromCache("tile", tilesId, "1")
			if (combinedTilesModel == null) {
				def profileMerger = new org.apache.maven.model.profile.DefaultProfileInjector.ProfileModelMerger();
				def merger = new DefaultInheritanceAssembler.InheritanceModelMerger() {
					@Override
					protected void mergeModel_Profiles( Model target, Model source, boolean sourceDominant,
							Map<Object, Object> context )
					{
						List<Profile> src = source.getProfiles();
						if ( !src.isEmpty() )
						{
							List<Profile> tgt = target.getProfiles();
							Map<Object, Profile> merged = new LinkedHashMap<>( ( src.size() + tgt.size() ) * 2 );

							for ( Profile element : tgt )
							{
								Object key = getProfileKey( element );
								merged.put( key, element );
							}

							for ( Profile element : src )
							{
								Object key = getProfileKey( element );
								if (merged.containsKey( key )) {
									mergeProfile( merged[key], element, sourceDominant, context) 
								} else {
									merged.put( key, element );
								}
							}

							target.setProfiles( new ArrayList<>( merged.values() ) );
						}
					}

					@Override
					protected Object getProfileKey(Profile profile) {
						return profile.getId();
					}

					@Override
					protected void mergeProfile( Profile target, Profile source, boolean sourceDominant, Map<Object, Object> context )
					{
						profileMerger.mergeModelBase( target, source );
						if ( source.getBuild() != null ) {
							if ( target.getBuild() == null ) {
								target.setBuild( source.getBuild().clone() );
							} else {
								profileMerger.mergeBuildBase( target.getBuild(), source.getBuild() );
							}
						}
						if ( source.getActivation() != null) {
							if ( target.getActivation() == null ) {
								target.setActivation( source.getActivation().clone() );
							} else {
								profileMerger.mergeActivation(target.getActivation(), source.getActivation(), false, context );
							}
						}
					}
				}

				// create the combined model
				tiles.each { TileModel tileModel ->
					Model model = tileModel.model
					if (combinedTilesModel == null) {
						combinedTilesModel = model.clone()
						combinedTilesModel.groupId = "tiles"
						combinedTilesModel.artifactId = tilesId
						combinedTilesModel.version = "1"
					} else {
						merger.merge(combinedTilesModel, tileModel.model, false, Collections.emptyMap())
					}
					logger.debug("Mixed '${modelGav(lastPom)}' with tile '${modelGav(model)}'.")
				}

				// put it into the cache
				putOriginalModelInCache("tile", tilesId, "1", combinedTilesModel, null)

				// link it into the structure
				Parent modelParent = new Parent(groupId: lastPom.groupId, version: lastPom.version,
					artifactId: lastPom.artifactId + "-tiles")
				lastPom.parent = modelParent
				putModelInCache(modelBuilder, lastPom, request, lastPomFile)

				combinedTilesModel.parent = originalParent
				putOriginalModelInCache(modelParent.groupId, modelParent.artifactId, modelParent.version, combinedTilesModel, null)
			} else {
				Parent modelParent = new Parent(groupId: lastPom.groupId, version: lastPom.version,
					artifactId: lastPom.artifactId + "-tiles")
				lastPom.parent = modelParent
				putModelInCache(modelBuilder, lastPom, request, lastPomFile)

				combinedTilesModel.parent = originalParent
				putOriginalModelInCache(modelParent.groupId, modelParent.artifactId, modelParent.version, combinedTilesModel, null)
			}

			// set a special property at the project so we can read out the list of applied tiles
			pomModel.properties[".applied-tiles"] = tiles.collect { tile -> GavUtil.modelGav(tile.model) }.join(",")

			logger.debug("")
		}
	}

	@CompileStatic(TypeCheckingMode.SKIP)
	protected void copyModel(MavenProject project, Model newModel) {

		// no setting parent, we have generated an effective model which is now all copied in
		Model projectModel = project.model

		// is there a change in dependencies ?
		boolean reresolveProjectArtifacts = false
		if (!project.resolvedArtifacts?.isEmpty() && !dependenciesEquals(projectModel.dependencies, newModel.dependencies)) {
			// resolved artifacts are invalid at this point
			project.resolvedArtifacts = null
			mavenVersionIsolate.flushProjectArtifacts(project)
			reresolveProjectArtifacts = true
		}

		projectModel.build = newModel.build
		projectModel.reporting = newModel.reporting
		projectModel.dependencyManagement = newModel.dependencyManagement
		projectModel.dependencies = newModel.dependencies
		projectModel.repositories = newModel.repositories
		projectModel.pluginRepositories = newModel.pluginRepositories
		projectModel.licenses = newModel.licenses
		projectModel.scm = newModel.scm
		projectModel.distributionManagement = newModel.distributionManagement
		projectModel.developers = newModel.developers
		projectModel.contributors = newModel.contributors
		projectModel.organization = newModel.organization
		projectModel.mailingLists = newModel.mailingLists
		projectModel.issueManagement = newModel.issueManagement
		projectModel.ciManagement = newModel.ciManagement
		projectModel.profiles = newModel.profiles
		projectModel.prerequisites = newModel.prerequisites
		projectModel.properties = newModel.properties

		// update model (test) source directory, which is the first entry and might have been set through a tile
		if (projectModel.build.sourceDirectory) {
			project.compileSourceRoots[0] = projectModel.build.sourceDirectory
		}
		if (projectModel.build.testSourceDirectory) {
			project.testCompileSourceRoots[0] = projectModel.build.testSourceDirectory
		}

		// for tile provided LifecycleMapping in m2e we need to modifiy the original model
		Plugin m2ePlugin = projectModel.build.pluginManagement?.getPluginsAsMap()?.get("org.eclipse.m2e:lifecycle-mapping")
		if (m2ePlugin) {
			Build build = project.originalModel.build
			if (!build) {
				build = new Build()
				project.originalModel.build = build
			}
			if (build.pluginManagement) {
				build.pluginManagement = build.pluginManagement.clone()
			} else {
				build.pluginManagement = new PluginManagement()
			}
			build.pluginManagement.addPlugin(m2ePlugin)
		}

		if (reresolveProjectArtifacts) {
			// re-resolving project artifacts needs to be done by invoking a private method
			DefaultProjectBuilder defaultProjectBuilder = (DefaultProjectBuilder) projectBuilder
			def resolveDependenciesMethod = DefaultProjectBuilder.class.getDeclaredMethod("resolveDependencies",
				(Class[]) [MavenProject.class, RepositorySystemSession.class])
			resolveDependenciesMethod.accessible = true
			resolveDependenciesMethod.invoke(projectBuilder, project, mavenSession.getRepositorySession())
		}
	}

	private static boolean dependenciesEquals( List<Dependency> a, List<Dependency> b )
	{
		if ( a.size() != b.size() )
		{
			return false
		}

		Iterator<Dependency> aI = a.iterator()
		Iterator<Dependency> bI = b.iterator()

		while ( aI.hasNext() )
		{
			Dependency aD = aI.next()
			Dependency bD = bI.next()

			boolean r = aD.groupId == bD.groupId && aD.artifactId == bD.artifactId && aD.version == bD.version && aD.type == bD.type && aD.classifier == bD.classifier && aD.scope == bD.scope

			r &= exclusionsEquals( aD.getExclusions(), bD.getExclusions() )

			if ( !r )
			{
				return false
			}
		}

		return true
	}

	private static boolean exclusionsEquals( List<Exclusion> a, List<Exclusion> b )
	{
		if ( a.size() != b.size() )
		{
			return false
		}

		Iterator<Exclusion> aI = a.iterator();
		Iterator<Exclusion> bI = b.iterator();

		while ( aI.hasNext() )
		{
			Exclusion aD = aI.next()
			Exclusion bD = bI.next()

			boolean r = aD.groupId == bD.groupId && aD.artifactId == bD.artifactId

			if ( !r )
			{
				return false
			}
		}

		return true
	}

	protected void loadAllDiscoveredTiles(MavenSession mavenSession, MavenProject project) throws MavenExecutionException {
		TileData tileData = getTileData(mavenSession)
		while (tileData.unprocessedTiles.size() > 0) {
			String unresolvedTile = tileData.unprocessedTiles.keySet().iterator().next()

			Artifact resolvedTile = resolveTile(mavenSession, project, tileData.unprocessedTiles.remove(unresolvedTile))

			TileModel tileModel = loadModel(resolvedTile)

			// ensure we have resolved the tile (it could come from a non-tile model)
			if (tileModel) {
				tileData.processedTiles.put(artifactName(resolvedTile), new ArtifactModel(resolvedTile, tileModel))
				parseForExtendedSyntax(mavenSession, project, tileModel, resolvedTile.getFile())
			}
		}

		ensureAllTilesDiscoveredAreAccountedFor(tileData)
	}

	/**
	 * removes all invalid tiles from the discovery order
	 */
	void ensureAllTilesDiscoveredAreAccountedFor(TileData tileData) {
		List<String> missingTiles = []

		tileData.tileDiscoveryOrder.each { String tile ->
			if (!tileData.processedTiles[tile]) {
				missingTiles.add(tile)
			}
		}

		tileData.tileDiscoveryOrder.removeAll(missingTiles)
	}

	/**
	 * Normally used inside the current project's pom file when declaring the tile plugin. People may prefer this
	 * to use to include tiles however in a tile.xml
	 */
	protected void parseConfiguration(MavenSession mavenSession, MavenProject project, Model model, File pomFile, boolean projectModel) {
		Xpp3Dom configuration = model?.build?.plugins?.
				find({ Plugin plugin ->
					return plugin.groupId == TILEPLUGIN_GROUP &&
							plugin.artifactId == TILEPLUGIN_ARTIFACT})?.configuration as Xpp3Dom

		Properties dependencyProperties = new Properties()
		if (configuration) {
			configuration.getChild("tiles")?.children?.each { Xpp3Dom tile ->
				processConfigurationTile(mavenSession, project, model, tile.value, pomFile)
			}
			applyBeforeParent = configuration.getChild("applyBefore")?.value
			mergeTarget = configuration.getChild("mergeTarget")?.value

			// empty value -> used to explicitly use the current project
			applyBeforeParent = applyBeforeParent?.empty ? null : applyBeforeParent
			mergeTarget = mergeTarget?.empty ? applyBeforeParent : mergeTarget
		}
	}

	/**
	 * Used for when we have a TileModel (we have read directly) so we support the extra syntax.
	 */
	protected void parseForExtendedSyntax(MavenSession mavenSession, MavenProject project, TileModel model, File pomFile) {
		model.tiles.each { String tileGav ->
			processConfigurationTile(mavenSession, project, model.model, tileGav, pomFile)
		}

		parseConfiguration(mavenSession, project, model.model, pomFile, false)
	}

	protected void processConfigurationTile(MavenSession mavenSession, MavenProject project, Model model, String tileDependencyName, File pomFile) {
		boolean removeFlag = false
		if (tileDependencyName.startsWith("!")) {
			// remove a tile if present
			tileDependencyName = tileDependencyName.drop(1)
			removeFlag = true
		}
		Artifact unprocessedTile = turnPropertyIntoUnprocessedTile(tileDependencyName, project, pomFile)
		TileData tileData = getTileData(mavenSession)
		String depName = artifactName(unprocessedTile)

		if (!tileData.processedTiles.containsKey(depName)) {
			if (tileData.unprocessedTiles.containsKey(depName)) {
				if (removeFlag) {
					logger.debug("Removing tile ${artifactGav(unprocessedTile)}")
					tileData.unprocessedTiles.remove(depName)
					tileData.tileDiscoveryOrder.remove(depName)
				} else {
					logger.warn(String.format("tiles-maven-plugin in project %s requested for same tile dependency %s",
							modelGav(model), artifactGav(unprocessedTile)))
				}
			} else if (!removeFlag) {
				logger.debug("Adding tile ${artifactGav(unprocessedTile)}")

				tileData.unprocessedTiles.put(depName, unprocessedTile)
				tileData.tileDiscoveryOrder.add(depName)
			}
		} else {
			logger.warn(String.format("tiles-maven-plugin in project %s requested for same tile dependency %s",
					modelGav(model), artifactGav(unprocessedTile)))
		}
	}

	class CachingModelSource implements ModelSource2  {
		Artifact pomArtifact
		File pomFile
		Model model

		CachingModelSource(Artifact pomArtifact, File pomFile) {
			this.pomArtifact = pomArtifact
			this.pomFile = pomFile
		}

		CachingModelSource(Artifact pomArtifact) {
			this(pomArtifact, pomArtifact.file)
		}

		CachingModelSource(File pomFile) {
			this.pomFile = pomFile
		}

		Artifact getPomArtifact() {
			return this.pomArtifact
		}

		@Override
		InputStream getInputStream() throws IOException {
			return pomFile.newInputStream()
		}

		@Override
		String getLocation() {
			return pomFile.absolutePath
		}

		@Override
		URI getLocationURI() {
			return pomFile.toURI()
		}

		Model getModel(ModelProcessor modelProcessor, Map<String, ?> options) {
			if (!model) {
				if (pomArtifact) {
					model = getOriginalModelFromCache(pomArtifact.groupId, pomArtifact.artifactId, pomArtifact.version)
				}
				if (!model) {
					model = modelProcessor.read(getInputStream(), options)
				}
			}
			return model.clone()
		}

		/**
		 * Only used for parents! We derive GAV from the current Model information.
		 */
		@Override
		ModelSource2 getRelatedSource( String relPath ) {
			File relatedPom = new File(pomFile.parentFile, relPath)
			if (relatedPom.isDirectory()) {
				relatedPom = new File(relatedPom, "pom.xml")
			}
			if (relatedPom.isFile() && relatedPom.canRead()) {
				if (model) {
					return new CachingModelSource(
							getArtifactFromCoordinates(GavUtil.getRealGroupId(model), model.getParent().artifactId, "pom", null, GavUtil.getRealVersion(model)),
							relatedPom)
				} else {
					return new CachingModelSource(relatedPom)
				}
			}
			return null
		}
	}
}
