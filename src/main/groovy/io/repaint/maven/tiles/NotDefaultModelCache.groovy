package io.repaint.maven.tiles

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.apache.maven.model.building.ModelCache
import org.eclipse.aether.RepositoryCache;
import org.eclipse.aether.RepositorySystemSession

import groovy.transform.CompileStatic

/**
 * Implementation of {@link ModelCache} which shares keys with org.apache.maven.repository.internal.DefaultModelCache
 * and caches the models in the {@link RepositoryCache} of the {@link RepositorySystemSession}.
 */
@CompileStatic
public class NotDefaultModelCache
    implements ModelCache
{
    private static Class DefaultModelCacheKey_class;
    private static Constructor DefaultModelCacheKey_new;

    static {
        try {
            DefaultModelCacheKey_class = Class.forName('org.apache.maven.repository.internal.DefaultModelCache$Key');
            DefaultModelCacheKey_new = DefaultModelCacheKey_class.getDeclaredConstructor(String.class, String.class, String.class, String.class);
			DefaultModelCacheKey_new.accessible = true;
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException exc) {
            throw new RuntimeException("Failed to access DefaultModelCache.Key", exc);
        }
    }

    private final RepositorySystemSession session;

    private final RepositoryCache cache;
    private final Map<Object, Object> localModels = new HashMap<>( 256 );

    public NotDefaultModelCache( RepositorySystemSession session )
    {
        this.session = session;
        this.cache = session.getCache();
    }

    public Object get( String groupId, String artifactId, String version, String tag )
    {
        Object key = createKey( groupId, artifactId, version, tag )
        if ( localModels.containsKey(key) ) {
            return localModels.get( key )
        }
        return cache.get( session, key );
    }

    public Object getOriginal( String groupId, String artifactId, String version, String tag )
    {
        Object key = createKey( groupId, artifactId, version, tag )
        return cache.get( session, key );
    }

    public void put( String groupId, String artifactId, String version, String tag, Object data )
    {
        localModels.putAt(createKey( groupId, artifactId, version, tag ), data);
    }

    public void putOriginal( String groupId, String artifactId, String version, String tag, Object data )
    {
        cache.put( session, createKey( groupId, artifactId, version, tag ), data);
    }

    static Object createKey( String groupId, String artifactId, String version, String tag ) {
        try {
            return DefaultModelCacheKey_new.newInstance(groupId, artifactId, version, tag);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException exc) {
            throw new RuntimeException("Failed to create DefaultModelCache.Key", exc);
		}
    }
}