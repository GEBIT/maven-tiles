package io.repaint.maven.tiles
import org.codehaus.plexus.logging.AbstractLogger
import org.codehaus.plexus.logging.Logger
/**
 *
 * @author: Erwin Tratar
 */
class DummyLogger extends AbstractLogger {

	DummyLogger(Logger delegate) {
		super(Logger.LEVEL_DISABLED, delegate.name)
	}

	// disabled
	@Override
	public void debug(String message, Throwable throwable) {}

	// disabled
	@Override
	public void info(String message, Throwable throwable) {}

	@Override
	public void warn(String message, Throwable throwable) {
		delegate.warn(message, throwable)
	}

	@Override
	public void error(String message, Throwable throwable) {
		delegate.error(message, throwable)
	}

	@Override
	public void fatalError(String message, Throwable throwable) {
		delegate.fatalError(message, throwable)
	}

	@Override
	public Logger getChildLogger(String name) {
		return this;
	}
}
